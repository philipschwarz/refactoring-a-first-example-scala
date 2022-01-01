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