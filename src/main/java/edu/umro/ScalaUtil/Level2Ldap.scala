/*
 * Copyright 2021 Regents of the University of Michigan
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package edu.umro.ScalaUtil

import java.util.Properties
import javax.naming.Context
import javax.naming.NamingEnumeration
import javax.naming.directory.InitialDirContext
import javax.naming.directory.SearchControls
import javax.naming.directory.SearchResult
import javax.naming.ldap.LdapName
import javax.naming.ldap.Rdn
import scala.collection.JavaConverters._

/**
  * Wrapper for LDAP service oriented for med.umich.
  *
  * Note that each call to the LDAP service opens and closes a connection, and so is
  * somewhat slow.  The caller should consider caching results to avoid unnecessary
  * overhead.
  *
  * @author irrer
  *
  */

object Level2Ldap {

  private val baseSearchContext = "dc=med,dc=umich,dc=edu"

  val umichMedUrl = "ldap://ldap.ent.med.umich.edu:636/" // production service
  // val umichMedUrl = "ldap://ldap2.med.umich.edu:636/" // production backup service
  // val umichMedUrl = "ldap://ldap.p-ent.med.umich.edu:636/" // pre-production service

  val ldapUrl: String = umichMedUrl

  def umichMedQuery(userId: String): String = "cn=" + userId + ",ou=people,dc=med,dc=umich,dc=edu"

  /**
    * Properties shared by all queries.
    */
  private def basicProperties: Properties = {
    val environment = new Properties

    environment.put(Context.INITIAL_CONTEXT_FACTORY, "com.sun.jndi.ldap.LdapCtxFactory")
    environment.put(Context.PROVIDER_URL, ldapUrl)
    environment.put(Context.SECURITY_PROTOCOL, "ssl")
    // don't dereference aliases because it puts too much load on LDAP servers
    environment.put("java.naming.ldap.derefAliases", "never")
    environment
  }

  private def openDirContext(environment: Properties): Either[String, InitialDirContext] = {
    try {
      val dc = new InitialDirContext(environment)
      if (dc == null)
        Left("Failed to connect to LDAP server " + ldapUrl)
      else
        Right(dc)
    } catch {
      case t: Throwable =>
        Left("Exception while to connecting to LDAP server " + ldapUrl + " : " + t)
    }
  }

  private def closeDirContext(dirContext: InitialDirContext): Unit = {
    try {
      dirContext.close()
    } catch {
      case _: Throwable => ;
    }
  }

  /**
    * Get list of the groups in the attribute list and set them to lower case.
    *
    * @param namingEnum LDAP data.
    *
    * @return List of the groups in the attribute list set to lower case.
    */
  private def extractGroupList(namingEnum: NamingEnumeration[SearchResult]): Either[String, Set[String]] = {
    try {
      val entryList = namingEnum.asScala.toList

      val attributesList = entryList.map(entry => entry.getAttributes)

      val attrList = attributesList.flatMap(attr => attr.getAll.asScala)

      val groupMem = attrList.filter { a => a.getID.equalsIgnoreCase("groupMembership") }

      val ldnList = groupMem.flatMap { gm => gm.getAll.asScala.toList }.map { x => new LdapName(x.toString) }

      val rdnList = ldnList.flatMap { ldn => ldn.getRdns.toArray.toList.map { o => o.asInstanceOf[Rdn] } }

      val groupList = rdnList.filter { rdn => rdn.getType.equalsIgnoreCase("cn") }.map { r => r.getValue.toString }.toSet

      Right(groupList)
    } catch {
      case t: Throwable =>
        Left("Unexpected exception while getting groups from LDAP reply: " + t)
    }
  }

  case class UserInfo(lastName: String, firstName: String, email: String) {
    override def toString: String = lastName + ", " + firstName + " at " + email
  }

  /**
    * Get list of the groups in the attribute list and set them to lower case.
    *
    * @param namingEnum LDAP data.
    *
    * @return List of the groups in the attribute list set to lower case.
    */
  private def extractUserInfo(namingEnum: NamingEnumeration[SearchResult]): Either[String, UserInfo] = {
    try {
      val entryList = namingEnum.asScala.toList

      val attributesList = entryList.map(entry => entry.getAttributes)

      val attrList = attributesList.flatMap(attr => attr.getAll.asScala)

      def get(tag: String): String = {
        attrList.find { a => a.getID.equalsIgnoreCase(tag) }.get.get.asInstanceOf[String]
      }

      val givenName = get("givenName")
      val sn = get("sn")
      val mail = get("mail")

      Right(UserInfo(sn, givenName, mail))
    } catch {
      case t: Throwable =>
        Left("Unexpected exception while getting groups from LDAP reply: " + t)
    }
  }

  /**
    * Get the list of groups that the user is a member of.
    *
    * @param userId User ID
    * @param secret Password.
    *
    * @return Either Left with error message or Right with list of groups.
    */
  def getGroupListOfUser(userId: String, secret: String): Either[String, Set[String]] = {

    val environment = basicProperties

    environment.put(Context.SECURITY_AUTHENTICATION, "simple")
    val securityPrincipal = umichMedQuery(userId)
    environment.put(Context.SECURITY_PRINCIPAL, securityPrincipal)
    // Note: If the following SECURITY_CREDENTIALS property is not set, then
    // it does not password authenticate.
    environment.put(Context.SECURITY_CREDENTIALS, secret)

    try {
      val dirContext = openDirContext(environment)
      if (dirContext.isRight) {
        val dc = dirContext.right.get

        val constraints = new SearchControls
        constraints.setSearchScope(SearchControls.SUBTREE_SCOPE)
        // set dereference aliases to put less load on server
        constraints.setDerefLinkFlag(false)

        val returnedAttributes = Array("groupMembership")
        constraints.setReturningAttributes(returnedAttributes)
        // construct the LDAP filter
        val filter = "uid=" + userId
        val nl = dc.search(baseSearchContext, filter, constraints)
        val groupList = extractGroupList(nl)
        closeDirContext(dc)
        groupList
      } else Left(dirContext.left.get)

    } catch {
      case t: Throwable => Left("Unexpected exception while getting connecting to LDAP server: " + t)

    }
  }

  /**
    * Get the list of groups that the user is a member of.
    *
    * @param userId User ID.
    * @param secret Password.
    *
    * @return Either Left with error message or Right with list of groups.
    */
  def getUserInfo(userId: String, secret: String): Either[String, UserInfo] = {
    try {
      val environment = basicProperties

      environment.put(Context.SECURITY_AUTHENTICATION, "simple")
      val securityPrincipal = umichMedQuery(userId)
      environment.put(Context.SECURITY_PRINCIPAL, securityPrincipal)
      // Note: If the following SECURITY_CREDENTIALS property is not set, then
      // it does not password authenticate.
      environment.put(Context.SECURITY_CREDENTIALS, secret)

      val dirContext = openDirContext(environment)
      if (dirContext.isRight) {
        val dc = dirContext.right.get

        val constraints = new SearchControls
        constraints.setSearchScope(SearchControls.SUBTREE_SCOPE)
        // set dereference aliases to put less load on server
        constraints.setDerefLinkFlag(false)

        val returnedAttributes = Array("sn", "givenName", "mail")
        constraints.setReturningAttributes(returnedAttributes)
        // construct the LDAP filter
        val filter = "uid=" + userId
        val nl = dc.search(baseSearchContext, filter, constraints)
        val userInfo = extractUserInfo(nl)
        closeDirContext(dc)
        userInfo
      } else Left(dirContext.left.get)

    } catch {
      case t: Throwable => Left("Unexpected exception while getting connecting to LDAP server: " + t)
    }
  }

  /**
    * Perform a health check to see if the authentication service is operational. Do
    * this by contacting the authentication service.
    *
    * @return None if service is operational, error message if unhealthy.
    */
  def healthCheck: Option[String] = {

    val dirContext = openDirContext(basicProperties)
    if (dirContext.isRight) {
      closeDirContext(dirContext.right.get)
      None
    } else Some(dirContext.left.get)

  }

  /**
    * Determine if user has entered correct password according to LDAP.
    *
    * @param userId User ID.
    * @param secret Password.
    *
    * @return True if correct password.
    */
  private def userIsAuthenticated(userId: String, secret: String): Boolean = {
    val environment = basicProperties

    environment.put(Context.SECURITY_AUTHENTICATION, "simple")
    val securityPrincipal = umichMedQuery(userId)
    environment.put(Context.SECURITY_PRINCIPAL, securityPrincipal)
    environment.put(Context.SECURITY_CREDENTIALS, secret)

    val dc = openDirContext(environment)
    if (dc.isRight) {
      closeDirContext(dc.right.get)
      true
    } else false
  }

  /**
    *  For testing only.
    */

  def main(args: Array[String]): Unit = {
    System.out.println("Starting.  Using URL " + ldapUrl)
    val health = Level2Ldap.healthCheck
    val msg = if (health.isDefined) "failed: " + health.get else "is healthy"
    println("LDAP health status: " + msg)

    val userId = System.getProperty("user.name")
    print("Enter Level 2 password for " + userId + " (and make sure no one is watching) : ")
    val secret = scala.io.StdIn.readLine

    println("authentication for user " + userId + " : " + Level2Ldap.userIsAuthenticated(userId, secret))

    println("\nUser: " + userId)
    val groups = Level2Ldap.getGroupListOfUser(userId, secret)
    if (groups.isRight) groups.right.get.toList.sorted.foreach { g => println("    " + g) }
    else println("  :(  no groups: " + groups.left.get)

    val userInfo = Level2Ldap.getUserInfo(userId, secret)
    println("user info: " + userInfo)

    System.out.println("Done")
    System.exit(0)
  }
}
