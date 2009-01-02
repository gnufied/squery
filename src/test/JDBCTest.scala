package test

import java.sql._
import scala.Array
import com.novocode.squery.simple._
import com.novocode.squery.simple.StaticQueryBase._
import com.novocode.squery.simple.Implicit._
import com.novocode.squery.session._
import com.novocode.squery.session.SessionFactory._


object JDBCTest {

  implicit def rsToUserOption(rs: ResultSet) =
    if(rs.isAfterLast) None else Some(new User(rs getInt 1, rs getString 2))

  implicit def rsToUser(rs: ResultSet) = new User(rs getInt 1, rs getString 2)

  def main(args : Array[String]) {

    val createTable = updateNA("create table USERS(ID int not null primary key, NAME varchar(255))")
    val populateUsers = updateNA("insert into USERS values(1, 'szeiger'), (0, 'admin'), (2, 'guest'); insert into USERS values(3, 'foo')")

    val allIDs = queryNA[Int]("select id from users")
    val userForID = query[Option[User],Int]("select id, name from users where id = ?")

    val sp = new DriverManagerSessionFactory("org.h2.Driver", "jdbc:h2:mem:test1")
    //val sp = new DriverManagerSessionFactory("org.h2.Driver", "jdbc:h2:tcp://localhost/test")
    //val sp = new DriverManagerSessionFactory("org.h2.Driver", "jdbc:h2:h2server/test")

    sp withSession {
      println("Creating user table: "+createTable())
      println("Inserting users:")
      for(i <- populateUsers) println("  "+i)

      println("All IDs:")
      for(s <- allIDs.list) println("  "+s)
      println("All IDs with foreach:")
      allIDs foreach (s => println("  "+s))
      val res = userForID(2)
      println("User for ID 2: "+res)
      println("User 2 with foreach:")
      userForID.prepare(2) foreach (s => println("  "+s) )
      println("User 2 with foreach:")
      GetUsers(Some(2)) foreach (s => println("  "+s))
      println("All users with foreach:")
      GetUsers(None) foreach (s => println("  "+s))
      println("All users with elements.foreach:")
      for(s <- GetUsers(None).elements) println("  "+s)
    } 
  }

  case class User(id:Int, name:String)

  case class GetUsers(id: Option[Int]) extends DynamicQuery[User] {
    select ~ "id, name from users"
    id foreach { this ~ "where id =" ~? _ }
  }

  case class GetUsers2(id: Option[Int]) extends DynamicQuery[User] {
    select ~ "id, name from users"
    wrap("where id =", "") { id foreach(v => this ~? v) }
  }
}
