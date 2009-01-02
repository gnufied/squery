package com.novocode.squery

import com.novocode.squery.session.{Session, CloseableIterator, ReadAheadIterator, PositionedParameters}
import com.novocode.squery.sql.InsertUpdateBuilder

class InsertUpdateInvoker[T] (column: ConvertableColumn[T]) {

  lazy val insertStatement = new InsertUpdateBuilder(column).buildInsert

  def insert(value: T)(implicit session: Session): Int = {
    val st = session.allocPS(insertStatement)
    st.clearParameters
    column.setParameter(new PositionedParameters(st), value)
    try { st.executeUpdate } finally session.freePS(insertStatement, st)
  }

  def insertAll(values: T*)(implicit session: Session): Int = (0 /: values) { _ + insert(_) }
}
