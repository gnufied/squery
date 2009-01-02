package com.novocode.squery.simple

import java.sql.{PreparedStatement, ResultSet}


/**
 * Base class for static queries, i.e. queries with a fixed query string.
 * They may still contain bind variables to be supplied at the call site.
 * The companion object contains utility methods for building static queries.
 */
abstract class StaticQueryBase[+T,-P](query: String, pconv: (P, PreparedStatement) => Unit) extends Query[T,P] {
  protected def queryString: String = query
  protected def setParam(param: P, st: PreparedStatement) = pconv(param, st)
  def prepare(param: P) = new ParameterizedQuery(this, param)
}

object StaticQueryBase {

  def query[T,P](query: String)(implicit rconv: ResultSet => T, pconv: (P, PreparedStatement) => Unit) =
    new StaticQuery[T,P](query, rconv, pconv)

  def queryNA[T](query: String)(implicit conv: ResultSet => T) =
    new StaticQuery[T,Unit](query, conv, Implicit.prepareFromUnit) with NoArgsQueryMixin[T]

  def update[P](query: String)(implicit pconv: (P, PreparedStatement) => Unit) =
    new StaticUpdate[P](query, pconv)

  def updateNA(query: String) =
    new StaticUpdate[Unit](query, Implicit.prepareFromUnit) with NoArgsQueryMixin[Int]
}


class StaticQuery[+T,-P](query: String, rconv: ResultSet => T, pconv: (P, PreparedStatement) => Unit)
  extends StaticQueryBase[T,P](query, pconv) with QueryQueryMixin[T,P] {
  protected def convertResult(rs: ResultSet): T = rconv(rs)
}


class StaticUpdate[-P](query: String, pconv: (P, PreparedStatement) => Unit)
  extends StaticQueryBase[Int,P](query, pconv) with UpdateQueryMixin[P]
