package com.novocode.squery.simple

import java.sql.{PreparedStatement, ResultSet, SQLException}
import com.novocode.squery.session._


/**
 * Base trait for all queries, using result type T and parameter type P
 */
trait Query[+T,-P] {
  
  def apply(param: P)(implicit session: Session): T

  def elements(param: P)(implicit session: Session): CloseableIterator[T]

  def list(param: P)(implicit session: Session): List[T] = {
    val it = elements(param)
    try { List.fromIterator(it) } finally { it.close }
  }

  def foreach(param: P, f: T => Unit)(implicit session: Session): Unit = {
    val it = elements(param)
    try { it.foreach(f) } finally { it.close }
  }
}


trait NoArgsQueryMixin[+T] extends Query[T,Unit] {
  final def apply()(implicit session: Session): T = apply(())
  final def list()(implicit session: Session): List[T] = list(())
  final def elements()(implicit session: Session): CloseableIterator[T] = elements(())
  final def foreach(f: T => Unit)(implicit session: Session): Unit = foreach((), f)
}


trait UpdateQueryMixin[-P] extends Query[Int,P] {

  protected def setParam(param: P, st: PreparedStatement)
  protected def queryString: String

  override def apply(param: P)(implicit session: Session): Int = {
    val st = session.allocPS(queryString)
    setParam(param, st)
    try { st.executeUpdate() } finally session.freePS(queryString, st)
  }

  override def elements(param: P)(implicit session: Session): CloseableIterator[Int] = {
    val st = session.allocPS(queryString)
    setParam(param, st)
    var doClose = true
    try {
      var hasRs = st.execute()
      var count = if(hasRs) 0 else st.getUpdateCount
      var first = true
      doClose = false
      new ReadAheadIterator[Int] with CloseableIterator[Int] {
        def close() = session.freePS(queryString, st)
        protected def fetchNext() = {
          if(first) first = false;
          else {
            hasRs = st.getMoreResults
            count = if(hasRs) 0 else st.getUpdateCount
          }
          if(count != -1) Some(count)
          else { close(); None }
        }
      }
    } finally if(doClose) session.freePS(queryString, st)
  }
}


trait QueryQueryMixin[+T,-P] extends Query[T,P] {

  protected def setParam(param: P, st: PreparedStatement)
  protected def queryString: String
  protected def convertResult(rs: ResultSet): T

  override def apply(param: P)(implicit session: Session): T = {
    val st = session.allocPS(queryString)
    setParam(param, st)
    try {
      val rs = st.executeQuery()
      rs.next
      convertResult(rs)
    } finally session.freePS(queryString, st)
  }

  override def list(param: P)(implicit session: Session): List[T] = {
    val st = session.allocPS(queryString)
    setParam(param, st)
    try {
      val rs = st.executeQuery()
      var xs:List[T] = Nil
      while(rs.next) xs = convertResult(rs) :: xs
      xs
    } finally session.freePS(queryString, st)
  }

  override def foreach(param: P, f: T => Unit)(implicit session: Session): Unit = {
    val st = session.allocPS(queryString)
    setParam(param, st)
    try {
      val rs = st.executeQuery()
      while(rs.next) f(convertResult(rs))
    } finally session.freePS(queryString, st)
  }

  override def elements(param: P)(implicit session: Session): CloseableIterator[T] = {
    val st = session.allocPS(queryString)
    setParam(param, st)
    var doClose = true
    try {
      val rs = st.executeQuery()
      doClose = false
      new ReadAheadIterator[T] with CloseableIterator[T] {
        def close() = session.freePS(queryString, st)
        protected def fetchNext() = {
          if(rs.next) Some(convertResult(rs))
          else { close(); None }
        }
      }
    } finally if(doClose) session.freePS(queryString, st)
  }

}


class ParameterizedQuery[+T,-P](query: Query[T,P], fixedParam: P) extends NoArgsQueryMixin[T] {
  override def apply(param: Unit)(implicit session: Session): T = query.apply(fixedParam)
  override def elements(param: Unit)(implicit session: Session): CloseableIterator[T] = query.elements(fixedParam)
  override def list(param: Unit)(implicit session: Session): List[T] = query.list(fixedParam)
  override def foreach(param: Unit, f: T => Unit)(implicit session: Session): Unit = query.foreach(fixedParam, f)
}
