package com.novocode.squery.simple

import java.sql.{PreparedStatement, ResultSet, SQLException, Date, Time, Timestamp}


/**
 * Implicit conversions for simple query parameters and query results
 */

object Implicit {

  // Implicit methods to convert results

  implicit def rsToBooleanOption(rs: ResultSet) = if(rs.isAfterLast) None else Some(rs getBoolean 1)
  implicit def rsToByteOption(rs: ResultSet) = if(rs.isAfterLast) None else Some(rs getByte 1)
  implicit def rsToDateOption(rs: ResultSet) = if(rs.isAfterLast) None else Some(rs getDate 1)
  implicit def rsToDoubleOption(rs: ResultSet) = if(rs.isAfterLast) None else Some(rs getDouble 1)
  implicit def rsToFloatOption(rs: ResultSet) = if(rs.isAfterLast) None else Some(rs getFloat 1)
  implicit def rsToIntOption(rs: ResultSet) = if(rs.isAfterLast) None else Some(rs getInt 1)
  implicit def rsToLongOption(rs: ResultSet) = if(rs.isAfterLast) None else Some(rs getLong 1)
  implicit def rsToShortOption(rs: ResultSet) = if(rs.isAfterLast) None else Some(rs getShort 1)
  implicit def rsToStringOption(rs: ResultSet) = if(rs.isAfterLast) None else Some(rs getString 1)
  implicit def rsToTimeOption(rs: ResultSet) = if(rs.isAfterLast) None else Some(rs getTime 1)
  implicit def rsToTimestampOption(rs: ResultSet) = if(rs.isAfterLast) None else Some(rs getTimestamp 1)

  implicit def rsToBoolean(rs: ResultSet) = rs getBoolean 1
  implicit def rsToByte(rs: ResultSet) = rs getByte 1
  implicit def rsToDate(rs: ResultSet) = rs getDate 1
  implicit def rsToDouble(rs: ResultSet) = rs getDouble 1
  implicit def rsToFloat(rs: ResultSet) = rs getFloat 1
  implicit def rsToInt(rs: ResultSet) = rs getInt 1
  implicit def rsToLong(rs: ResultSet) = rs getLong 1
  implicit def rsToShort(rs: ResultSet) = rs getShort 1
  implicit def rsToString(rs: ResultSet) = rs getString 1
  implicit def rsToTime(rs: ResultSet) = rs getTime 1
  implicit def rsToTimestamp(rs: ResultSet) = rs getTimestamp 1


  // Implicit methods to convert parameters

  implicit def prepareFromBoolean(v: Boolean, st: PreparedStatement) { st.setBoolean(1, v) }
  implicit def prepareFromByte(v: Byte, st: PreparedStatement) { st.setByte(1, v) }
  implicit def prepareFromDate(v: Date, st: PreparedStatement) { st.setDate(1, v) }
  implicit def prepareFromDouble(v: Double, st: PreparedStatement) { st.setDouble(1, v) }
  implicit def prepareFromFloat(v: Float, st: PreparedStatement) { st.setFloat(1, v) }
  implicit def prepareFromInt(v: Int, st: PreparedStatement) { st.setInt(1, v) }
  implicit def prepareFromLong(v: Long, st: PreparedStatement) { st.setLong(1, v) }
  implicit def prepareFromShort(v: Short, st: PreparedStatement) { st.setShort(1, v) }
  implicit def prepareFromString(v: String, st: PreparedStatement) { st.setString(1, v) }
  implicit def prepareFromTime(v: Time, st: PreparedStatement) { st.setTime(1, v) }
  implicit def prepareFromTimestamp(v: Timestamp, st: PreparedStatement) { st.setTimestamp(1, v) }

  implicit def prepareFromProduct(prod: Product, st: PreparedStatement): Unit =
    for(i <- 0 until prod.productArity) prod.productElement(i) match {
      case v: Boolean => st.setBoolean(i+1, v)
      case v: Byte => st.setByte(i+1, v)
      case v: Date => st.setDate(i+1, v)
      case v: Double => st.setDouble(i+1, v)
      case v: Float => st.setFloat(i+1, v)
      case v: Int => st.setInt(i+1, v)
      case v: Long => st.setLong(i+1, v)
      case v: Short => st.setShort(i+1, v)
      case v: String => st.setString(i+1, v)
      case v: Time => st.setTime(i+1, v)
      case v: Timestamp => st.setTimestamp(i+1, v)
      case v => throw new SQLException("prepareFromProduct doesn't know how to handle parameter "+i+"( "+v+")")
    }

  implicit def prepareFromUnit(none: Unit, st: PreparedStatement) = ()
}
