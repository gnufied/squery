package com.novocode.squery

object Implicit {
  implicit def columnOfBooleanToBooleanColumn(c: Column[Boolean]): BooleanColumn = c match {
    case c: BooleanColumn => c
    case _ => new WrappedColumn(c) with BooleanColumn
  }

  implicit def columnOfIntToIntColumn(c: Column[Int]): IntColumn = c match {
    case c: IntColumn => c
    case _ => new WrappedColumn(c) with IntColumn
  }

  implicit def columnOfStringToStringColumn(c: Column[String]): StringColumn = c match {
    case c: StringColumn => c
    case _ => new WrappedColumn(c) with StringColumn
  }

  implicit def intToConstColumn(v: Int) = new ConstColumn(java.lang.Integer.valueOf(v)) with IntColumn
  implicit def integerToConstColumn(v: java.lang.Integer) = new ConstColumn(v) with IntColumn
  implicit def stringToConstColumn(v: String) = new ConstColumn(v) with StringColumn

  implicit def tableToQuery[T <: TableBase.T_](t: T) = Query(t.withOp(new ColumnOp.BaseTableQueryOp(t)))

  // Not implicit to work around bug #1579
  def queryToSubQuery[C <: Column.T_](q: Query[C]): C = q.value.withOp(ColumnOp.SubQueryOp(q))

  implicit def queryToQueryInvoker[T](q: Query[ConvertableColumn[T]]): QueryInvoker[T,T] = QueryInvoker(q)
  implicit def queryToDeleteInvoker[T](q: Query[Table[T]]): DeleteInvoker[T] = new DeleteInvoker(q)
  implicit def tableToDDLInvoker[T](t: Table[T]): DDLInvoker[T] = new DDLInvoker(t)
  implicit def convertableColumnToInsertUpdateInvoker[T](c: ConvertableColumn[T]) = new InsertUpdateInvoker(c)
}
