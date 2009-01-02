package com.novocode.squery

import com.novocode.squery.session.Session
import com.novocode.squery.sql.DDLBuilder

class DDLInvoker[T](table: Table[T]) {

  lazy val createTableStatement = new DDLBuilder(table).buildCreateTable

  def createTable(implicit session: Session) {
    val st = session.allocPS(createTableStatement)
    try { st.execute } finally session.freePS(createTableStatement, st)
  }
}
