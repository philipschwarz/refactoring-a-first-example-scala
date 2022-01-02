case class Play(name: String, `type`: String)

case class Performance(playID: String, audience: Int)

case class EnrichedPerformance(
  playID: String,
  play: Play,
  audience: Int,
  amount: Int,
  volumeCredits: Int)

case class Invoice(customer: String, performances: List[Performance])

case class StatementData(
  customer: String,
  performances: List[EnrichedPerformance],
  totalAmount: Int,
  totalVolumeCredits: Int)

sealed trait PerformanceCalculator:
  def performance: Performance
  def play: Play
  def amount: Int
  def volumeCredits: Int = math.max(performance.audience - 30, 0)

case class TragedyCalculator(performance: Performance, play: Play) extends PerformanceCalculator:
  def amount: Int =
    val basicAmount = 40_000
    val largeAudiencePremiumAmount = if performance.audience <= 30 then 0 else 1_000 * (performance.audience - 30)
    basicAmount + largeAudiencePremiumAmount

case class ComedyCalculator(performance: Performance, play: Play) extends PerformanceCalculator:
  def amount: Int =
    val basicAmount = 30_000
    val largeAudiencePremiumAmount = if performance.audience <= 20 then 0 else 10_000 + 500 * (performance.audience - 20)
    val audienceSizeAmount = 300 * performance.audience
    basicAmount + largeAudiencePremiumAmount + audienceSizeAmount  
  override def volumeCredits: Int =
    super.volumeCredits + math.floor(performance.audience / 5).toInt

object PerformanceCalculator:
  def apply(aPerformance: Performance, aPlay: Play): PerformanceCalculator =
    aPlay.`type` match
      case "tragedy" => TragedyCalculator(aPerformance, aPlay)
      case "comedy" => ComedyCalculator(aPerformance, aPlay)
      case other => throw IllegalArgumentException(s"unknown type ${aPlay.`type`}")