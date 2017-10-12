package com.rayrobdod.possibleEvolutions
package evolutionData

import scala.collection.immutable.Map

object AlphaSapphire extends SeedData {
	def game:EvosGame.Value = EvosGame.AlphaSapphire
	
	lazy val evolutions:Map[DexNo, Map[String, DexNo]] = {
		val builder = new DexNo.DexNoMapBuilder[Map[String, DexNo]]
		
		builder += ((DexNo( 63), Map("Level Up [16]" -> DexNo( 65))))
		builder += ((DexNo(190), Map("Level Up with Move [Double Hit]" -> DexNo(428))))
		builder += ((DexNo(698), Map("Level Up at Night [39]" -> DexNo(639))))
		builder += ((DexNo(347), Map("Level Up [40]" -> DexNo(127))))
		builder += ((DexNo(566), Map("Level Up [37]" -> DexNo(697))))
		builder += ((DexNo(304), Map("Level Up [32]" -> DexNo(681))))
		builder += ((DexNo(610), Map("Level Up [38]" -> DexNo(633))))
		builder += ((DexNo(298), Map("Level Up with Friendship" -> DexNo(270))))
		builder += ((DexNo(371), Map("Level Up [30]" -> DexNo(610))))
		builder += ((DexNo(343), Map("Level Up [36]" -> DexNo(389))))
		builder += ((DexNo(339), Map("Level Up [30]" -> DexNo( 75))))
		builder += ((DexNo(153), Map("Level Up [32]" -> DexNo(673))))
		builder += ((DexNo(374), Map("Level Up [20]" -> DexNo(561))))
		builder += ((DexNo( 69), Map("Level Up [21]" -> DexNo(496))))
		builder += ((DexNo(712), Map("Level Up [37]" -> DexNo(215))))
		builder += ((DexNo(399), Map("Level Up [15]" -> DexNo(206))))
		builder += ((DexNo(688), Map("Level Up [39]" -> DexNo(272))))
		builder += ((DexNo(522), Map("Level Up [27]" -> DexNo(695))))
		builder += ((DexNo(525), Map("Trade" -> DexNo(142))))
		builder += ((DexNo(438), Map("Level Up with Move [Mimic]" -> DexNo(222))))
		builder += ((DexNo(654), Map("Level Up [36]" -> DexNo(687))))
		builder += ((DexNo(436), Map("Level Up [33]" -> DexNo(681))))
		builder += ((DexNo(406), Map("Level Up at Morning" -> DexNo(545))))
		builder += ((DexNo(418), Map("Level Up [26]" -> DexNo(537))))
		builder += ((DexNo(  1), Map("Level Up [16]" -> DexNo(182))))
		builder += ((DexNo(427), Map("Level Up with Friendship" -> DexNo(520))))
		builder += ((DexNo(659), Map("Level Up [20]" -> DexNo( 20))))
		builder += ((DexNo(412), Map("Level Up Female [20]" -> DexNo(549), "Level Up Male [20]" -> DexNo(413))))
		builder += ((DexNo(331), Map("Level Up [32]" -> DexNo(470))))
		builder += ((DexNo(318), Map("Level Up [30]" -> DexNo(121))))
		builder += ((DexNo(268), Map("Level Up [10]" -> DexNo( 72))))
		builder += ((DexNo( 10), Map("Level Up [7]" -> DexNo( 14))))
		builder += ((DexNo(113), Map("Level Up with Friendship" -> DexNo( 40))))
		builder += ((DexNo(  4), Map("Level Up [16]" -> DexNo(229))))
		builder += ((DexNo(  5), Map("Level Up [36]" -> DexNo(373))))
		builder += ((DexNo(420), Map("Level Up [25]" -> DexNo( 44))))
		builder += ((DexNo(650), Map("Level Up [16]" -> DexNo(542))))
		builder += ((DexNo(152), Map("Level Up [16]" -> DexNo(455))))
		builder += ((DexNo(390), Map("Level Up [14]" -> DexNo(126))))
		builder += ((DexNo(170), Map("Level Up [27]" -> DexNo(321))))
		builder += ((DexNo(433), Map("Level Up at Night" -> DexNo( 65))))
		builder += ((DexNo(366), Map("Trade with Held Item [Deep Sea Tooth]" -> DexNo(139), "Trade with Held Item [Deep Sea Scale]" -> DexNo(131))))
		builder += ((DexNo(692), Map("Level Up [37]" -> DexNo( 99))))
		builder += ((DexNo( 35), Map("Used Item [Moon Stone]" -> DexNo(122))))
		builder += ((DexNo(173), Map("Level Up with Friendship" -> DexNo(209))))
		builder += ((DexNo(415), Map("Level Up Female [21]" -> DexNo(277))))
		builder += ((DexNo(256), Map("Level Up [36]" -> DexNo( 62))))
		builder += ((DexNo(341), Map("Level Up [30]" -> DexNo(442))))
		builder += ((DexNo(546), Map("Used Item [Sun Stone]" -> DexNo(598))))
		builder += ((DexNo(408), Map("Level Up [30]" -> DexNo(464))))
		builder += ((DexNo(453), Map("Level Up [37]" -> DexNo( 57))))
		builder += ((DexNo(159), Map("Level Up [30]" -> DexNo(647))))
		builder += ((DexNo(613), Map("Level Up [37]" -> DexNo(215))))
		builder += ((DexNo(104), Map("Level Up [28]" -> DexNo(660))))
		builder += ((DexNo(155), Map("Level Up [14]" -> DexNo(324))))
		builder += ((DexNo(554), Map("Level Up [35]" -> DexNo(257))))
		builder += ((DexNo(585), Map("Level Up [34]" -> DexNo(460))))
		builder += ((DexNo(633), Map("Level Up [50]" -> DexNo(715))))
		builder += ((DexNo(502), Map("Level Up [36]" -> DexNo(516))))
		builder += ((DexNo( 50), Map("Level Up [26]" -> DexNo(330))))
		builder += ((DexNo( 84), Map("Level Up [31]" -> DexNo(169))))
		builder += ((DexNo(680), Map("Used Item [Dusk Stone]" -> DexNo(462))))
		builder += ((DexNo(148), Map("Level Up [55]" -> DexNo(384))))
		builder += ((DexNo(147), Map("Level Up [30]" -> DexNo(705))))
		builder += ((DexNo(425), Map("Level Up [28]" -> DexNo(178))))
		builder += ((DexNo(529), Map("Level Up [31]" -> DexNo(376))))
		builder += ((DexNo( 96), Map("Level Up [26]" -> DexNo(282))))
		builder += ((DexNo(580), Map("Level Up [35]" -> DexNo(342))))
		builder += ((DexNo(578), Map("Level Up [41]" -> DexNo(687))))
		builder += ((DexNo(356), Map("Trade with Held Item [Reaper Cloth]" -> DexNo(609))))
		builder += ((DexNo(355), Map("Level Up [37]" -> DexNo(593))))
		builder += ((DexNo(557), Map("Level Up [34]" -> DexNo(469))))
		builder += ((DexNo(603), Map("Used Item [Thunder Stone]" -> DexNo(243))))
		builder += ((DexNo(133), Map(
				"Level Up at Forest" -> DexNo( 44), "Level Up at Cold" -> DexNo(460),
				"Used Item [Thunder Stone]" -> DexNo(417), "Used Item [Water Stone]" -> DexNo( 99),
				"Used Item [Fire Stone]" -> DexNo(654), "Level Up with 50 Affection + MoveType [Fairy]" -> DexNo(707),
				"Level Up at Morning" -> DexNo(606), "Level Up at Night" -> DexNo(430))))
		builder += ((DexNo( 23), Map("Level Up [22]" -> DexNo(407))))
		builder += ((DexNo(125), Map("Trade with Held Item [Electirizer]" -> DexNo(479))))
		builder += ((DexNo(309), Map("Level Up [26]" -> DexNo(135))))
		builder += ((DexNo(239), Map("Level Up [30]" -> DexNo(604))))
		builder += ((DexNo(605), Map("Level Up [42]" -> DexNo(480))))
		builder += ((DexNo(677), Map("Level Up Male [25]" -> DexNo(178), "Level Up Female (SetForm 1) [25]" -> DexNo(103))))
		builder += ((DexNo(102), Map("Used Item [Leaf Stone]" -> DexNo(709))))
		builder += ((DexNo(349), Map("Level Up (Beauty) [170]" -> DexNo(581), "Trade with Held Item [Prism Scale]" -> DexNo( 87))))
		builder += ((DexNo(653), Map("Level Up [16]" -> DexNo(391))))
		builder += ((DexNo(597), Map("Level Up [40]" -> DexNo(332))))
		builder += ((DexNo(456), Map("Level Up [31]" -> DexNo(340))))
		builder += ((DexNo(180), Map("Level Up [30]" -> DexNo(125))))
		builder += ((DexNo(669), Map("Level Up [19]" -> DexNo(702))))
		builder += ((DexNo(662), Map("Level Up [35]" -> DexNo(164))))
		builder += ((DexNo(661), Map("Level Up [17]" -> DexNo(255))))
		builder += ((DexNo(670), Map("Used Item [Shiny Stone]" -> DexNo(703))))
		builder += ((DexNo(590), Map("Level Up [39]" -> DexNo( 45))))
		builder += ((DexNo(611), Map("Level Up [48]" -> DexNo(381))))
		builder += ((DexNo(592), Map("Level Up [40]" -> DexNo(477))))
		builder += ((DexNo(656), Map("Level Up [16]" -> DexNo(457))))
		builder += ((DexNo(657), Map("Level Up [36]" -> DexNo(368))))
		builder += ((DexNo(444), Map("Level Up [48]" -> DexNo(232))))
		builder += ((DexNo( 92), Map("Level Up [25]" -> DexNo( 15))))
		builder += ((DexNo( 74), Map("Level Up [25]" -> DexNo(409))))
		builder += ((DexNo(443), Map("Level Up [24]" -> DexNo(111))))
		builder += ((DexNo(431), Map("Level Up [38]" -> DexNo(398))))
		builder += ((DexNo(207), Map("Level Up with Held Item (Night) [Razor Fang]" -> DexNo(189))))
		builder += ((DexNo( 44), Map("Used Item [Leaf Stone]" -> DexNo(407), "Used Item [Sun Stone]" -> DexNo(455))))
		builder += ((DexNo( 42), Map("Level Up with Friendship" -> DexNo(207))))
		builder += ((DexNo(118), Map("Level Up [33]" -> DexNo(550))))
		builder += ((DexNo(622), Map("Level Up [43]" -> DexNo( 75))))
		builder += ((DexNo(704), Map("Level Up [40]" -> DexNo(718))))
		builder += ((DexNo(574), Map("Level Up [32]" -> DexNo(528))))
		builder += ((DexNo(575), Map("Level Up [41]" -> DexNo(203))))
		builder += ((DexNo( 75), Map("Trade" -> DexNo(232))))
		builder += ((DexNo( 88), Map("Level Up [38]" -> DexNo( 45))))
		builder += ((DexNo(388), Map("Level Up [32]" -> DexNo( 76))))
		builder += ((DexNo(253), Map("Level Up [36]" -> DexNo(598))))
		builder += ((DexNo( 58), Map("Used Item [Fire Stone]" -> DexNo(146))))
		builder += ((DexNo(316), Map("Level Up [26]" -> DexNo(  3))))
		builder += ((DexNo(533), Map("Trade" -> DexNo(107))))
		builder += ((DexNo(440), Map("Level Up with Held Item (Day) [Oval Stone]" -> DexNo(288))))
		builder += ((DexNo( 93), Map("Trade" -> DexNo(336))))
		builder += ((DexNo(694), Map("Used Item [Sun Stone]" -> DexNo(398))))
		builder += ((DexNo(507), Map("Level Up [32]" -> DexNo(628))))
		builder += ((DexNo(449), Map("Level Up [34]" -> DexNo(537))))
		builder += ((DexNo(679), Map("Level Up [35]" -> DexNo(477))))
		builder += ((DexNo(163), Map("Level Up [20]" -> DexNo(295))))
		builder += ((DexNo(187), Map("Level Up [18]" -> DexNo(291))))
		builder += ((DexNo(116), Map("Level Up [32]" -> DexNo(689))))
		builder += ((DexNo(228), Map("Level Up [24]" -> DexNo(485))))
		builder += ((DexNo(174), Map("Level Up with Friendship" -> DexNo( 84))))
		builder += ((DexNo(686), Map("Level Up with 3DS Upside Down [30]" -> DexNo(342))))
		builder += ((DexNo(  2), Map("Level Up [32]" -> DexNo(407))))
		builder += ((DexNo( 39), Map("Used Item [Moon Stone]" -> DexNo(352))))
		builder += ((DexNo(595), Map("Level Up [36]" -> DexNo(469))))
		builder += ((DexNo(140), Map("Level Up [40]" -> DexNo(121))))
		builder += ((DexNo( 64), Map("Trade" -> DexNo(196))))
		builder += ((DexNo( 14), Map("Level Up [10]" -> DexNo(284))))
		builder += ((DexNo(588), Map("Trade for opposite Karrablast/Shelmet" -> DexNo(707))))
		builder += ((DexNo(281), Map("Level Up [30]" -> DexNo(719), "Level Up with Held Item (Male) [Dawn Stone]" -> DexNo(539))))
		builder += ((DexNo(600), Map("Level Up [49]" -> DexNo(625))))
		builder += ((DexNo(599), Map("Level Up [38]" -> DexNo(448))))
		builder += ((DexNo(109), Map("Level Up [35]" -> DexNo(569))))
		builder += ((DexNo( 98), Map("Level Up [28]" -> DexNo(658))))
		builder += ((DexNo(401), Map("Level Up [10]" -> DexNo(313))))
		builder += ((DexNo(552), Map("Level Up [40]" -> DexNo(450))))
		builder += ((DexNo(305), Map("Level Up [42]" -> DexNo(649))))
		builder += ((DexNo(608), Map("Used Item [Dusk Stone]" -> DexNo(136))))
		builder += ((DexNo(636), Map("Level Up [59]" -> DexNo(205))))
		builder += ((DexNo(246), Map("Level Up [30]" -> DexNo(529))))
		builder += ((DexNo(165), Map("Level Up [18]" -> DexNo(580))))
		builder += ((DexNo(108), Map("Level Up with Move [Rollout]" -> DexNo(628))))
		builder += ((DexNo(345), Map("Level Up [40]" -> DexNo(  2))))
		builder += ((DexNo(506), Map("Level Up [16]" -> DexNo(505))))
		builder += ((DexNo(667), Map("Level Up [35]" -> DexNo(288))))
		builder += ((DexNo(607), Map("Level Up [41]" -> DexNo(126))))
		builder += ((DexNo(271), Map("Used Item [Water Stone]" -> DexNo(141))))
		builder += ((DexNo(270), Map("Level Up [14]" -> DexNo(  7))))
		builder += ((DexNo(294), Map("Level Up [40]" -> DexNo(335))))
		builder += ((DexNo(404), Map("Level Up [30]" -> DexNo(642))))
		builder += ((DexNo( 67), Map("Trade" -> DexNo( 57))))
		builder += ((DexNo( 66), Map("Level Up [28]" -> DexNo(237))))
		builder += ((DexNo(240), Map("Level Up [30]" -> DexNo(609))))
		builder += ((DexNo(129), Map("Level Up [20]" -> DexNo(272))))
		builder += ((DexNo(126), Map("Trade with Held Item [Magmarizer]" -> DexNo(392))))
		builder += ((DexNo( 81), Map("Level Up [30]" -> DexNo(479))))
		builder += ((DexNo( 82), Map("Level Up at Electric" -> DexNo(625))))
		builder += ((DexNo(296), Map("Level Up [24]" -> DexNo(214))))
		builder += ((DexNo( 56), Map("Level Up [28]" -> DexNo(701))))
		builder += ((DexNo(458), Map("Level up with Party [Remoraid]" -> DexNo(561))))
		builder += ((DexNo(179), Map("Level Up [15]" -> DexNo(100))))
		builder += ((DexNo(183), Map("Level Up [18]" -> DexNo(  8))))
		builder += ((DexNo(259), Map("Level Up [36]" -> DexNo(171))))
		builder += ((DexNo(307), Map("Level Up [37]" -> DexNo(103))))
		builder += ((DexNo( 52), Map("Level Up [28]" -> DexNo(520))))
		builder += ((DexNo(375), Map("Level Up [45]" -> DexNo(385))))
		builder += ((DexNo( 11), Map("Level Up [10]" -> DexNo(166))))
		builder += ((DexNo(619), Map("Level Up [50]" -> DexNo(308))))
		builder += ((DexNo(439), Map("Level Up with Move [Mimic]" -> DexNo( 40))))
		builder += ((DexNo(572), Map("Used Item [Shiny Stone]" -> DexNo( 85))))
		builder += ((DexNo(200), Map("Used Item [Dusk Stone]" -> DexNo(681))))
		builder += ((DexNo(391), Map("Level Up [36]" -> DexNo( 59))))
		builder += ((DexNo(258), Map("Level Up [16]" -> DexNo(657))))
		builder += ((DexNo(446), Map("Level Up with Friendship" -> DexNo(233))))
		builder += ((DexNo(517), Map("Used Item [Moon Stone]" -> DexNo(579))))
		builder += ((DexNo(198), Map("Used Item [Dusk Stone]" -> DexNo(435))))
		builder += ((DexNo(177), Map("Level Up [25]" -> DexNo(326))))
		builder += ((DexNo( 29), Map("Level Up [16]" -> DexNo(690))))
		builder += ((DexNo( 32), Map("Level Up [16]" -> DexNo( 24))))
		builder += ((DexNo( 30), Map("Used Item [Moon Stone]" -> DexNo( 76))))
		builder += ((DexNo( 33), Map("Used Item [Moon Stone]" -> DexNo(569))))
		builder += ((DexNo(290), Map("Level Up (Ninjask) [20]" -> DexNo(627))))
		builder += ((DexNo(714), Map("Level Up [48]" -> DexNo(178))))
		builder += ((DexNo(299), Map("Level Up at Electric" -> DexNo(589))))
		builder += ((DexNo(322), Map("Level Up [33]" -> DexNo(553))))
		builder += ((DexNo(274), Map("Used Item [Leaf Stone]" -> DexNo(357))))
		builder += ((DexNo( 43), Map("Level Up [21]" -> DexNo(586))))
		builder += ((DexNo(138), Map("Level Up [40]" -> DexNo(199))))
		builder += ((DexNo( 95), Map("Trade with Held Item [Metal Coat]" -> DexNo(632))))
		builder += ((DexNo(501), Map("Level Up [17]" -> DexNo(117))))
		builder += ((DexNo(536), Map("Level Up [36]" -> DexNo( 55))))
		builder += ((DexNo(674), Map("Dark Type in Party [32]" -> DexNo(107))))
		builder += ((DexNo(515), Map("Used Item [Water Stone]" -> DexNo(581))))
		builder += ((DexNo(511), Map("Used Item [Leaf Stone]" -> DexNo(332))))
		builder += ((DexNo(513), Map("Used Item [Fire Stone]" -> DexNo(663))))
		builder += ((DexNo( 46), Map("Level Up [24]" -> DexNo(666))))
		builder += ((DexNo(504), Map("Level Up [20]" -> DexNo(203))))
		builder += ((DexNo(624), Map("Level Up [52]" -> DexNo(687))))
		builder += ((DexNo(548), Map("Used Item [Sun Stone]" -> DexNo(154))))
		builder += ((DexNo(231), Map("Level Up [25]" -> DexNo(260))))
		builder += ((DexNo(708), Map("Trade" -> DexNo(  3))))
		builder += ((DexNo(172), Map("Level Up with Friendship" -> DexNo(595))))
		builder += ((DexNo( 17), Map("Level Up [36]" -> DexNo(695))))
		builder += ((DexNo( 16), Map("Level Up [18]" -> DexNo( 83))))
		builder += ((DexNo(519), Map("Level Up [21]" -> DexNo(108))))
		builder += ((DexNo(499), Map("Level Up [36]" -> DexNo(539))))
		builder += ((DexNo( 25), Map("Used Item [Thunder Stone]" -> DexNo(596))))
		builder += ((DexNo(221), Map("Level Up with Move [Ancient Power]" -> DexNo(389))))
		builder += ((DexNo(204), Map("Level Up [31]" -> DexNo(127))))
		builder += ((DexNo(393), Map("Level Up [16]" -> DexNo(  8))))
		builder += ((DexNo( 60), Map("Level Up [25]" -> DexNo(117))))
		builder += ((DexNo( 61), Map("Used Item [Water Stone]" -> DexNo(620), "Trade with Held Item [King's Rock]" -> DexNo(139))))
		builder += ((DexNo( 77), Map("Level Up [40]" -> DexNo(663))))
		builder += ((DexNo(261), Map("Level Up [18]" -> DexNo(461))))
		builder += ((DexNo(137), Map("Trade with Held Item [Up-Grade]" -> DexNo(424))))
		builder += ((DexNo(233), Map("Trade with Held Item [Dubious Disc]" -> DexNo(128))))
		builder += ((DexNo(394), Map("Level Up [36]" -> DexNo(589))))
		builder += ((DexNo( 54), Map("Level Up [33]" -> DexNo(272))))
		builder += ((DexNo(710), Map("Trade" -> DexNo(609))))
		builder += ((DexNo(247), Map("Level Up [55]" -> DexNo(658))))
		builder += ((DexNo(509), Map("Level Up [20]" -> DexNo(302))))
		builder += ((DexNo(156), Map("Level Up [36]" -> DexNo(  6))))
		builder += ((DexNo(651), Map("Level Up [36]" -> DexNo(500))))
		builder += ((DexNo(280), Map("Level Up [20]" -> DexNo(374))))
		builder += ((DexNo( 19), Map("Level Up [20]" -> DexNo(264))))
		builder += ((DexNo(223), Map("Level Up [25]" -> DexNo( 62))))
		builder += ((DexNo(112), Map("Trade with Held Item [Protector]" -> DexNo(260))))
		builder += ((DexNo(111), Map("Level Up [42]" -> DexNo(369))))
		builder += ((DexNo(447), Map("Level Up at Morning" -> DexNo(530))))
		builder += ((DexNo(524), Map("Level Up [25]" -> DexNo(185))))
		builder += ((DexNo(315), Map("Used Item [Shiny Stone]" -> DexNo(598))))
		builder += ((DexNo(627), Map("Level Up [54]" -> DexNo(663))))
		builder += ((DexNo(551), Map("Level Up [29]" -> DexNo(629))))
		builder += ((DexNo( 27), Map("Level Up [22]" -> DexNo(340))))
		builder += ((DexNo(664), Map("Level Up [9]" -> DexNo(401))))
		builder += ((DexNo(559), Map("Level Up [39]" -> DexNo(461))))
		builder += ((DexNo(123), Map("Trade with Held Item [Metal Coat]" -> DexNo(617))))
		builder += ((DexNo(117), Map("Trade with Held Item [Dragon Scale]" -> DexNo(706))))
		builder += ((DexNo(364), Map("Level Up [44]" -> DexNo(503))))
		builder += ((DexNo(273), Map("Level Up [14]" -> DexNo(672))))
		builder += ((DexNo( 86), Map("Level Up [34]" -> DexNo(689))))
		builder += ((DexNo(161), Map("Level Up [15]" -> DexNo(667))))
		builder += ((DexNo(496), Map("Level Up [36]" -> DexNo(709))))
		builder += ((DexNo(540), Map("Level Up [20]" -> DexNo(252))))
		builder += ((DexNo(372), Map("Level Up [50]" -> DexNo( 85))))
		builder += ((DexNo( 90), Map("Used Item [Water Stone]" -> DexNo(419))))
		builder += ((DexNo(422), Map("Level Up [30]" -> DexNo(321))))
		builder += ((DexNo(616), Map("Trade for opposite Karrablast/Shelmet" -> DexNo(205))))
		builder += ((DexNo(410), Map("Level Up [30]" -> DexNo(639))))
		builder += ((DexNo(403), Map("Level Up [15]" -> DexNo(171))))
		builder += ((DexNo(285), Map("Level Up [23]" -> DexNo(  2))))
		builder += ((DexNo(353), Map("Level Up [37]" -> DexNo(563))))
		builder += ((DexNo(266), Map("Level Up [10]" -> DexNo( 18))))
		builder += ((DexNo(672), Map("Level Up [32]" -> DexNo(652))))
		builder += ((DexNo(188), Map("Level Up [27]" -> DexNo(455))))
		builder += ((DexNo(300), Map("Used Item [Moon Stone]" -> DexNo(531))))
		builder += ((DexNo(451), Map("Level Up [40]" -> DexNo(691))))
		builder += ((DexNo(690), Map("Level Up [48]" -> DexNo( 94))))
		builder += ((DexNo(287), Map("Level Up [18]" -> DexNo(128))))
		builder += ((DexNo(705), Map("Overworld Rain [50]" -> DexNo(644))))
		builder += ((DexNo( 79), Map("Level Up [37]" -> DexNo(134), "Trade with Held Item [King's Rock]" -> DexNo(581))))
		builder += ((DexNo(218), Map("Level Up [38]" -> DexNo(185))))
		builder += ((DexNo(238), Map("Level Up [30]" -> DexNo(199))))
		builder += ((DexNo(215), Map("Level Up with Held Item (Night) [Razor Claw]" -> DexNo(124))))
		builder += ((DexNo(495), Map("Level Up [17]" -> DexNo(586))))
		builder += ((DexNo(361), Map("Level Up [42]" -> DexNo(131), "Level Up with Held Item (Female) [Dawn Stone]" -> DexNo(711))))
		builder += ((DexNo(459), Map("Level Up [40]" -> DexNo(673))))
		builder += ((DexNo(209), Map("Level Up [23]" -> DexNo(683))))
		builder += ((DexNo(577), Map("Level Up [32]" -> DexNo(325))))
		builder += ((DexNo( 21), Map("Level Up [20]" -> DexNo(469))))
		builder += ((DexNo(665), Map("Level Up [12]" -> DexNo(284))))
		builder += ((DexNo(363), Map("Level Up [32]" -> DexNo(400))))
		builder += ((DexNo(167), Map("Level Up [22]" -> DexNo(291))))
		builder += ((DexNo(325), Map("Level Up [32]" -> DexNo(437))))
		builder += ((DexNo(682), Map("Trade with Held Item [Sachet]" -> DexNo( 40))))
		builder += ((DexNo(  7), Map("Level Up [16]" -> DexNo( 99))))
		builder += ((DexNo(397), Map("Level Up [34]" -> DexNo(414))))
		builder += ((DexNo(396), Map("Level Up [14]" -> DexNo(165))))
		builder += ((DexNo(120), Map("Used Item [Water Stone]" -> DexNo(395))))
		builder += ((DexNo(434), Map("Level Up [34]" -> DexNo(569))))
		builder += ((DexNo(191), Map("Used Item [Sun Stone]" -> DexNo(591))))
		builder += ((DexNo(283), Map("Level Up [22]" -> DexNo(567))))
		builder += ((DexNo(333), Map("Level Up [35]" -> DexNo(193))))
		builder += ((DexNo(541), Map("Level Up with Friendship" -> DexNo(103))))
		builder += ((DexNo(220), Map("Level Up [33]" -> DexNo(259))))
		builder += ((DexNo(684), Map("Trade with Held Item [Whipped Dream]" -> DexNo(468))))
		builder += ((DexNo(276), Map("Level Up [22]" -> DexNo(207))))
		builder += ((DexNo(114), Map("Level Up with Move [Ancient Power]" -> DexNo(  3))))
		builder += ((DexNo(216), Map("Level Up [30]" -> DexNo(508))))
		builder += ((DexNo( 72), Map("Level Up [30]" -> DexNo(593))))
		builder += ((DexNo(498), Map("Level Up [17]" -> DexNo(391))))
		builder += ((DexNo(532), Map("Level Up [25]" -> DexNo(559))))
		builder += ((DexNo(564), Map("Level Up [37]" -> DexNo(279))))
		builder += ((DexNo(175), Map("Level Up with Friendship" -> DexNo(177))))
		builder += ((DexNo(176), Map("Used Item [Shiny Stone]" -> DexNo(145))))
		builder += ((DexNo(255), Map("Level Up [16]" -> DexNo(608))))
		builder += ((DexNo(158), Map("Level Up [18]" -> DexNo(117))))
		builder += ((DexNo(520), Map("Level Up [32]" -> DexNo(143))))
		builder += ((DexNo(328), Map("Level Up [35]" -> DexNo(704))))
		builder += ((DexNo(252), Map("Level Up [16]" -> DexNo(496))))
		builder += ((DexNo(568), Map("Level Up [36]" -> DexNo( 31))))
		builder += ((DexNo(387), Map("Level Up [18]" -> DexNo(407))))
		builder += ((DexNo(535), Map("Level Up [25]" -> DexNo(224))))
		builder += ((DexNo(602), Map("Level Up [39]" -> DexNo(642))))
		builder += ((DexNo(236), Map("Level Up (Attack < Defense) [20]" -> DexNo(237), "Level Up (Attack > Defense) [20]" -> DexNo(620), "Level Up (Attack = Defense) [20]" -> DexNo( 62))))
		builder += ((DexNo(696), Map("Level Up at Night [39]" -> DexNo(635))))
		builder += ((DexNo(583), Map("Level Up [47]" -> DexNo(460))))
		builder += ((DexNo(582), Map("Level Up [35]" -> DexNo(364))))
		builder += ((DexNo(543), Map("Level Up [22]" -> DexNo(168))))
		builder += ((DexNo( 48), Map("Level Up [31]" -> DexNo( 70))))
		builder += ((DexNo(329), Map("Level Up [45]" -> DexNo(334))))
		builder += ((DexNo(288), Map("Level Up [36]" -> DexNo(508))))
		builder += ((DexNo(100), Map("Level Up [30]" -> DexNo(596))))
		builder += ((DexNo(629), Map("Level Up [54]" -> DexNo(461))))
		builder += ((DexNo( 37), Map("Used Item [Fire Stone]" -> DexNo(555))))
		builder += ((DexNo(320), Map("Level Up [40]" -> DexNo(658))))
		builder += ((DexNo(  8), Map("Level Up [36]" -> DexNo(489))))
		builder += ((DexNo( 13), Map("Level Up [7]" -> DexNo(665))))
		builder += ((DexNo( 70), Map("Used Item [Leaf Stone]" -> DexNo(154))))
		builder += ((DexNo(544), Map("Level Up [30]" -> DexNo( 47))))
		builder += ((DexNo(293), Map("Level Up [20]" -> DexNo( 40))))
		builder += ((DexNo(278), Map("Level Up [25]" -> DexNo(593))))
		builder += ((DexNo(527), Map("Level Up with Friendship" -> DexNo(358))))
		builder += ((DexNo(194), Map("Level Up [20]" -> DexNo( 27))))
		builder += ((DexNo(265), Map("Level Up (Random < 5) [7]" -> DexNo(265), "Level Up (Random > 5) [7]" -> DexNo( 14))))
		builder += ((DexNo(360), Map("Level Up [15]" -> DexNo(575))))
		builder += ((DexNo(562), Map("Level Up [34]" -> DexNo(623))))
		builder += ((DexNo(193), Map("Level Up with Move [Ancient Power]" -> DexNo(558))))
		builder += ((DexNo(263), Map("Level Up [20]" -> DexNo( 83))))
		builder += ((DexNo(570), Map("Level Up [30]" -> DexNo(452))))
		builder += ((DexNo( 41), Map("Level Up [22]" -> DexNo(169))))
		builder += ((DexNo(634), Map("Level Up [64]" -> DexNo(373))))

		builder.withDefault(Map.empty)
		builder.result
	}
}
