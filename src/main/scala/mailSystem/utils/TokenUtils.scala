package mailSystem.utils

object TokenUtils {
  def generateToken(): String = {
    val token = java.util.UUID.randomUUID().toString + System.currentTimeMillis()
    token
  }
}
