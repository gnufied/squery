package com.novocode.squery

import java.sql.ResultSet
import com.novocode.squery.session.{Session, CloseableIterator, ReadAheadIterator, PositionedResult}
import com.novocode.squery.sql.QueryBuilder

class QueryInvoker[T,R] private (q: Query[ConvertableColumn[T]], mapper: T => R) {

  lazy val selectStatement = new QueryBuilder(q).buildSelect

  def convertResult(rs: PositionedResult): R = {
    val qr = q.value.getResult(rs)
    mapper(qr)
  }

  def first(implicit session: Session): Option[R] = {
    var res: Option[R] = None
    foreach({ x => res = Some(x) }, 1)
    res
  }

  def list(implicit session: Session): List[R] = {
    var xs:List[R] = Nil
    foreach({ x => xs = x :: xs }, 0)
    xs
  }

  def foreach(f: R => Unit)(implicit session: Session): Unit = foreach(f, 0)

  private[this] def foreach(f: R => Unit, maxRows: Int)(implicit session: Session): Unit = {
    //TODO Support multiple results
    val st = session.allocPS(selectStatement)
    try {
      st.setMaxRows(maxRows)
      if(st.execute) {
        var count = 0
        val rs = new PositionedResult(st.getResultSet)
        while(rs.next && (maxRows == 0 || count < maxRows)) {
          f(convertResult(rs))
          count += 1
        }
      } else f(mapper(st.getUpdateCount.asInstanceOf[T]))
    } finally session.freePS(selectStatement, st)
  }

  def elements(implicit session: Session): CloseableIterator[R] = {
    //TODO Support multiple results
    val st = session.allocPS(selectStatement)
    var doClose = true
    try {
      st.setMaxRows(0)
      if(st.execute) {
        val rs = new PositionedResult(st.getResultSet)
        doClose = false
        new ReadAheadIterator[R] with CloseableIterator[R] {
          def close() = session.freePS(selectStatement, st)
          protected def fetchNext() = {
            if(rs.next) Some(convertResult(rs))
            else { close(); None }
          }
        }
      } else {
        val r = mapper(st.getUpdateCount.asInstanceOf[T])
        new CloseableIterator[R] {
          private var hasnext = true
          def hasNext: Boolean = hasnext
          def next(): R =
            if (hasnext) { hasnext = false; r }
            else throw new NoSuchElementException("next on empty iterator")
          def close {}
        }
      }
    } finally if(doClose) session.freePS(selectStatement, st)
  }

  def mapResult[U](f: (R => U)) = new QueryInvoker[T,U](q, { v:T => f(mapper(v)) })
}

object QueryInvoker {
  def apply[T](q: Query[ConvertableColumn[T]]) = new QueryInvoker[T,T](q, { v:T => v })
}
