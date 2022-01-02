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
  def volumeCredits: Int =
    var result = 0
    result += math.max(performance.audience - 30, 0)
    if "comedy" == play.`type` then result += math.floor(performance.audience / 5).toInt
    result
case class TragedyCalculator(performance: Performance, play: Play) extends PerformanceCalculator:
  def amount: Int =
    var result = 0
    result = 40_000
    if performance.audience > 30 then result += 1_000 * (performance.audience - 30)
    result
case class ComedyCalculator(performance: Performance, play: Play) extends PerformanceCalculator:
  def amount: Int =
    var result = 30_000
    if performance.audience > 20 then result += 10_000 + 500 * (performance.audience - 20)
    result += 300 * performance.audience
    result
object PerformanceCalculator:
  def apply(aPerformance: Performance, aPlay: Play): PerformanceCalculator =
    aPlay.`type` match
      case "tragedy" => TragedyCalculator(aPerformance, aPlay)
      case "comedy" => ComedyCalculator(aPerformance, aPlay)
      case other => throw IllegalArgumentException(s"unknown type ${aPlay.`type`}")