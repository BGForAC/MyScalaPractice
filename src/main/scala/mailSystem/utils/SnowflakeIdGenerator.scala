package mailSystem.utils

class SnowflakeIdGenerator(datacenterId: Long, workerId: Long) {
  private val epoch = 1609459200000L
  val datacenterIdBits = 5L
  val workerIdBits = 5L
  val sequenceBits = 12L

  private val maxDatacenterId = ~(-1L << datacenterIdBits)
  private val maxWorkerId = ~(-1L << workerIdBits)
  private val maxSequence = ~(-1L << sequenceBits)

  private val datacenterIdShift = sequenceBits
  private val workerIdShift = sequenceBits + datacenterIdBits
  private val timestampLeftShift = sequenceBits + datacenterIdBits + workerIdBits

  private var sequence = 0L
  private var lastTimestamp = -1L

  require(datacenterId <= maxDatacenterId && datacenterId >= 0, s"Datacenter ID can't be greater than $maxDatacenterId or less than 0")
  require(workerId <= maxWorkerId && workerId >= 0, s"Worker ID can't be greater than $maxWorkerId or less than 0")

  def nextId(): Long = synchronized {
    val timestamp = timeGen()
    if (timestamp < lastTimestamp) {
      throw new RuntimeException(s"Clock moved backwards. Refusing to generate id for ${lastTimestamp - timestamp} milliseconds")
    }

    if (lastTimestamp == timestamp) {
      sequence = (sequence + 1) & maxSequence
      if (sequence == 0) {
        tilNextMillis(lastTimestamp)
      }
    } else {
      sequence = 0
    }

    lastTimestamp = timestamp

    ((timestamp - epoch) << timestampLeftShift) |
      (datacenterId << datacenterIdShift) |
      (workerId << workerIdShift) |
      sequence
  }

  private def tilNextMillis(lastTimestamp: Long): Long = {
    var timestamp = timeGen()
    while (timestamp <= lastTimestamp) {
      timestamp = timeGen()
    }
    timestamp
  }

  private def timeGen(): Long = System.currentTimeMillis()
}
