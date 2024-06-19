package edu.umro.ScalaUtil

import gnu.crypto.Registry
import gnu.crypto.hash.HashFactory

object Crypto {

  val hashChoices = {
    Seq(
      Registry.WHIRLPOOL_HASH,
      Registry.RIPEMD128_HASH,
      Registry.RIPEMD160_HASH,
      Registry.SHA160_HASH,
      Registry.SHA256_HASH,
      Registry.SHA384_HASH,
      Registry.SHA512_HASH,
      Registry.TIGER_HASH,
      Registry.HAVAL_HASH,
      Registry.MD5_HASH,
      Registry.MD4_HASH,
      Registry.MD2_HASH
    )
  }

  val defaultHash = Registry.MD5_HASH

  /**
    * Construct a hash from the given data.  Secure in this context means it
    * is not computationally viable to reverse.
    *
    * @param data Source data.
    * @return Secure hash of data.
    */
  def hash(data: Array[Byte], digestName: String = defaultHash): Array[Byte] = {
    val md = HashFactory.getInstance(digestName)
    md.update(data, 0, data.length)
    md.digest
  }
}
