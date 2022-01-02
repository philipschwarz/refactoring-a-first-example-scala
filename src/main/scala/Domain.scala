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

case class PerformanceCalculator(performance: Performance, play: Play):
  def amount: Int =
    var result = 0
    play.`type` match
      case "tragedy" =>
        result = 40_000
        if performance.audience > 30
        then result += 1_000 * (performance.audience - 30)
      case "comedy" =>
        result = 30_000
        if performance.audience > 20
        then result += 10_000 + 500 * (performance.audience - 20)
        result += 300 * performance.audience
      case other =>
        throw IllegalArgumentException(s"unknown type ${play.`type`}")
    result
  