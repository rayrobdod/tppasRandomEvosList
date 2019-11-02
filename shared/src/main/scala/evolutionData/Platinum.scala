package com.rayrobdod.possibleEvolutions
package evolutionData

import scala.collection.immutable.{Seq, Map}

object Platinum extends SeedData {
	lazy val evolutions:Map[DexNo, Map[String, DexNo]] = {
		val builder = Map.newBuilder[DexNo, Map[String, DexNo]]
		
		builder += ((DexNo.national(1), Map("Level Up [16]" -> DexNo.national(5))))
		builder += ((DexNo.national(2), Map("Level Up [32]" -> DexNo.national(3))))
		builder += ((DexNo.national(4), Map("Level Up [16]" -> DexNo.national(5))))
		builder += ((DexNo.national(5), Map("Level Up [36]" -> DexNo.national(395))))
		builder += ((DexNo.national(7), Map("Level Up [16]" -> DexNo.national(8))))
		builder += ((DexNo.national(8), Map("Level Up [36]" -> DexNo.national(94))))
		builder += ((DexNo.national(10), Map("Level Up [7]" -> DexNo.national(11))))
		builder += ((DexNo.national(11), Map("Level Up [10]" -> DexNo.national(262))))
		builder += ((DexNo.national(13), Map("Level Up [7]" -> DexNo.national(161))))
		builder += ((DexNo.national(14), Map("Level Up [10]" -> DexNo.national(137))))
		builder += ((DexNo.national(16), Map("Level Up [18]" -> DexNo.national(17))))
		builder += ((DexNo.national(17), Map("Level Up [36]" -> DexNo.national(31))))
		builder += ((DexNo.national(19), Map("Level Up [20]" -> DexNo.national(414))))
		builder += ((DexNo.national(21), Map("Level Up [20]" -> DexNo.national(308))))
		builder += ((DexNo.national(23), Map("Level Up [22]" -> DexNo.national(51))))
		builder += ((DexNo.national(25), Map("Used Item [Thunder Stone]" -> DexNo.national(125))))
		builder += ((DexNo.national(27), Map("Level Up [22]" -> DexNo.national(202))))
		builder += ((DexNo.national(29), Map("Level Up [16]" -> DexNo.national(30))))
		builder += ((DexNo.national(30), Map("Used Item [Moon Stone]" -> DexNo.national(65))))
		builder += ((DexNo.national(32), Map("Level Up [16]" -> DexNo.national(258))))
		builder += ((DexNo.national(33), Map("Used Item [Moon Stone]" -> DexNo.national(260))))
		builder += ((DexNo.national(35), Map("Used Item [Moon Stone]" -> DexNo.national(424))))
		builder += ((DexNo.national(37), Map("Used Item [Fire Stone]" -> DexNo.national(419))))
		builder += ((DexNo.national(39), Map("Used Item [Moon Stone]" -> DexNo.national(40))))
		builder += ((DexNo.national(41), Map("Level Up [22]" -> DexNo.national(77))))
		builder += ((DexNo.national(42), Map("Level Up with Friendship" -> DexNo.national(169))))
		builder += ((DexNo.national(43), Map("Level Up [21]" -> DexNo.national(61))))
		builder += ((DexNo.national(44), Map("Used Item [Leaf Stone]" -> DexNo.national(215), "Used Item [Sun Stone]" -> DexNo.national(392))))
		builder += ((DexNo.national(46), Map("Level Up [24]" -> DexNo.national(47))))
		builder += ((DexNo.national(48), Map("Level Up [31]" -> DexNo.national(53))))
		builder += ((DexNo.national(50), Map("Level Up [26]" -> DexNo.national(308))))
		builder += ((DexNo.national(52), Map("Level Up [28]" -> DexNo.national(119))))
		builder += ((DexNo.national(54), Map("Level Up [33]" -> DexNo.national(471))))
		builder += ((DexNo.national(56), Map("Level Up [28]" -> DexNo.national(442))))
		builder += ((DexNo.national(58), Map("Used Item [Fire Stone]" -> DexNo.national(445))))
		builder += ((DexNo.national(60), Map("Level Up [25]" -> DexNo.national(44))))
		builder += ((DexNo.national(61), Map("Used Item [Water Stone]" -> DexNo.national(392), "Trade with Held Item [King's Rock]" -> DexNo.national(461))))
		builder += ((DexNo.national(63), Map("Level Up [16]" -> DexNo.national(64))))
		builder += ((DexNo.national(64), Map("Trade" -> DexNo.national(461))))
		builder += ((DexNo.national(66), Map("Level Up [28]" -> DexNo.national(207))))
		builder += ((DexNo.national(67), Map("Trade" -> DexNo.national(68))))
		builder += ((DexNo.national(69), Map("Level Up [21]" -> DexNo.national(61))))
		builder += ((DexNo.national(70), Map("Used Item [Leaf Stone]" -> DexNo.national(71))))
		builder += ((DexNo.national(72), Map("Level Up [30]" -> DexNo.national(73))))
		builder += ((DexNo.national(74), Map("Level Up [25]" -> DexNo.national(75))))
		builder += ((DexNo.national(75), Map("Trade" -> DexNo.national(76))))
		builder += ((DexNo.national(77), Map("Level Up [40]" -> DexNo.national(99))))
		builder += ((DexNo.national(79), Map("Level Up [37]" -> DexNo.national(115),"Trade with Held Item [King's Rock]" -> DexNo.national(85))))
		builder += ((DexNo.national(81), Map("Level Up [30]" -> DexNo.national(279))))
		builder += ((DexNo.national(82), Map("Level Up at Electric" -> DexNo.national(97))))
		builder += ((DexNo.national(84), Map("Level Up [31]" -> DexNo.national(85))))
		builder += ((DexNo.national(86), Map("Level Up [34]" -> DexNo.national(442))))
		builder += ((DexNo.national(88), Map("Level Up [38]" -> DexNo.national(237))))
		builder += ((DexNo.national(90), Map("Used Item [Water Stone]" -> DexNo.national(450))))
		builder += ((DexNo.national(92), Map("Level Up [25]" -> DexNo.national(398))))
		builder += ((DexNo.national(93), Map("Trade" -> DexNo.national(472))))
		builder += ((DexNo.national(95), Map("Trade with Held Item [Metal Coat]" -> DexNo.national(208))))
		builder += ((DexNo.national(96), Map("Level Up [26]" -> DexNo.national(203))))
		builder += ((DexNo.national(98), Map("Level Up [28]" -> DexNo.national(122))))
		builder += ((DexNo.national(100), Map("Level Up [30]" -> DexNo.national(471))))
		builder += ((DexNo.national(102), Map("Used Item [Leaf Stone]" -> DexNo.national(103))))
		builder += ((DexNo.national(104), Map("Level Up [28]" -> DexNo.national(105))))
		builder += ((DexNo.national(108), Map("Level Up with Move [Rollout]" -> DexNo.national(463))))
		builder += ((DexNo.national(109), Map("Level Up [35]" -> DexNo.national(99))))
		builder += ((DexNo.national(111), Map("Level Up [42]" -> DexNo.national(171))))
		builder += ((DexNo.national(112), Map("Trade with Held Item [Protector]" -> DexNo.national(481))))
		builder += ((DexNo.national(113), Map("Level Up with Friendship" -> DexNo.national(242))))
		builder += ((DexNo.national(114), Map("Level Up with Move [Ancient Power]" -> DexNo.national(474))))
		builder += ((DexNo.national(116), Map("Level Up [32]" -> DexNo.national(22))))
		builder += ((DexNo.national(117), Map("Trade with Held Item [Dragon Scale]" -> DexNo.national(196))))
		builder += ((DexNo.national(118), Map("Level Up [33]" -> DexNo.national(53))))
		builder += ((DexNo.national(120), Map("Used Item [Water Stone]" -> DexNo.national(59))))
		builder += ((DexNo.national(123), Map("Trade with Held Item [Metal Coat]" -> DexNo.national(28))))
		builder += ((DexNo.national(125), Map("Trade with Held Item [Electirizer]" -> DexNo.national(89))))
		builder += ((DexNo.national(126), Map("Trade with Held Item [Magmarizer]" -> DexNo.national(465))))
		builder += ((DexNo.national(129), Map("Level Up [20]" -> DexNo.national(130))))
		builder += ((DexNo.national(133), scala.collection.immutable.ListMap(
				"Used Item [Water Stone]" -> DexNo.national(476), "Used Item [Thunder Stone]" -> DexNo.national(101),
				"Used Item [Fire Stone]" -> DexNo.national(97),
				"Level Up at Morning with Friendship" -> DexNo.national(462), "Level Up at Night with Friendship" -> DexNo.national(136),
				"Level Up at Forest" -> DexNo.national(470), "Level Up at Cold" -> DexNo.national(203)
		)))
		builder += ((DexNo.national(137), Map("Trade with Held Item [Up-Grade]" -> DexNo.national(232))))
		builder += ((DexNo.national(138), Map("Level Up [40]" -> DexNo.national(139))))
		builder += ((DexNo.national(140), Map("Level Up [40]" -> DexNo.national(421))))
		builder += ((DexNo.national(147), Map("Level Up [30]" -> DexNo.national(369))))
		builder += ((DexNo.national(148), Map("Level Up [55]" -> DexNo.national(369))))
		builder += ((DexNo.national(152), Map("Level Up [16]" -> DexNo.national(256))))
		builder += ((DexNo.national(153), Map("Level Up [32]" -> DexNo.national(405))))
		builder += ((DexNo.national(155), Map("Level Up [14]" -> DexNo.national(352))))
		builder += ((DexNo.national(156), Map("Level Up [36]" -> DexNo.national(68))))
		builder += ((DexNo.national(158), Map("Level Up [18]" -> DexNo.national(253))))
		builder += ((DexNo.national(159), Map("Level Up [30]" -> DexNo.national(160))))
		builder += ((DexNo.national(161), Map("Level Up [15]" -> DexNo.national(162))))
		builder += ((DexNo.national(163), Map("Level Up [20]" -> DexNo.national(162))))
		builder += ((DexNo.national(165), Map("Level Up [18]" -> DexNo.national(166))))
		builder += ((DexNo.national(167), Map("Level Up [22]" -> DexNo.national(168))))
		builder += ((DexNo.national(170), Map("Level Up [27]" -> DexNo.national(372))))
		builder += ((DexNo.national(172), Map("Level Up with Friendship" -> DexNo.national(422))))
		builder += ((DexNo.national(173), Map("Level Up with Friendship" -> DexNo.national(35))))
		builder += ((DexNo.national(174), Map("Level Up with Friendship" -> DexNo.national(183))))
		builder += ((DexNo.national(175), Map("Level Up with Friendship" -> DexNo.national(303))))
		builder += ((DexNo.national(176), Map("Used Item [Shiny Stone]" -> DexNo.national(468))))
		builder += ((DexNo.national(177), Map("Level Up [25]" -> DexNo.national(421))))
		builder += ((DexNo.national(179), Map("Level Up [15]" -> DexNo.national(180))))
		builder += ((DexNo.national(180), Map("Level Up [30]" -> DexNo.national(181))))
		builder += ((DexNo.national(183), Map("Level Up [18]" -> DexNo.national(184))))
		builder += ((DexNo.national(187), Map("Level Up [18]" -> DexNo.national(402))))
		builder += ((DexNo.national(188), Map("Level Up [27]" -> DexNo.national(416))))
		builder += ((DexNo.national(190), Map("Level Up with Move [Double Hit]" -> DexNo.national(424))))
		builder += ((DexNo.national(191), Map("Used Item [Sun Stone]" -> DexNo.national(277))))
		builder += ((DexNo.national(193), Map("Level Up with Move [Ancient Power]" -> DexNo.national(469))))
		builder += ((DexNo.national(194), Map("Level Up [20]" -> DexNo.national(195))))
		builder += ((DexNo.national(198), Map("Used Item [Dusk Stone]" -> DexNo.national(257))))
		builder += ((DexNo.national(200), Map("Used Item [Dusk Stone]" -> DexNo.national(429))))
		builder += ((DexNo.national(204), Map("Level Up [31]" -> DexNo.national(279))))
		builder += ((DexNo.national(207), Map("Level Up with Held Item (Night) [Razor Fang]" -> DexNo.national(68))))
		builder += ((DexNo.national(209), Map("Level Up [23]" -> DexNo.national(338))))
		builder += ((DexNo.national(215), Map("Level Up with Held Item (Night) [Razor Claw]" -> DexNo.national(461))))
		builder += ((DexNo.national(216), Map("Level Up [30]" -> DexNo.national(217))))
		builder += ((DexNo.national(218), Map("Level Up [38]" -> DexNo.national(400))))
		builder += ((DexNo.national(220), Map("Level Up [33]" -> DexNo.national(221))))
		builder += ((DexNo.national(221), Map("Level Up with Move [Ancient Power]" -> DexNo.national(241))))
		builder += ((DexNo.national(223), Map("Level Up [25]" -> DexNo.national(53))))
		builder += ((DexNo.national(228), Map("Level Up [24]" -> DexNo.national(282))))
		builder += ((DexNo.national(231), Map("Level Up [25]" -> DexNo.national(465))))
		builder += ((DexNo.national(233), Map("Trade with Held Item [Dubious Disc]" -> DexNo.national(442))))
		builder += ((DexNo.national(236), Map("Level Up (Attack > Defense) [20]" -> DexNo.national(193), "Level Up (Attack < Defense) [20]" -> DexNo.national(312), "Level Up (Attack = Defense) [20]" -> DexNo.national(24))))
		builder += ((DexNo.national(238), Map("Level Up [30]" -> DexNo.national(421))))
		builder += ((DexNo.national(239), Map("Level Up [30]" -> DexNo.national(139))))
		builder += ((DexNo.national(240), Map("Level Up [30]" -> DexNo.national(126))))
		builder += ((DexNo.national(246), Map("Level Up [30]" -> DexNo.national(226))))
		builder += ((DexNo.national(247), Map("Level Up [55]" -> DexNo.national(59))))
		builder += ((DexNo.national(252), Map("Level Up [16]" -> DexNo.national(159))))
		builder += ((DexNo.national(253), Map("Level Up [36]" -> DexNo.national(181))))
		builder += ((DexNo.national(255), Map("Level Up [16]" -> DexNo.national(198))))
		builder += ((DexNo.national(256), Map("Level Up [36]" -> DexNo.national(405))))
		builder += ((DexNo.national(258), Map("Level Up [16]" -> DexNo.national(259))))
		builder += ((DexNo.national(259), Map("Level Up [36]" -> DexNo.national(34))))
		builder += ((DexNo.national(261), Map("Level Up [18]" -> DexNo.national(164))))
		builder += ((DexNo.national(263), Map("Level Up [20]" -> DexNo.national(24))))
		builder += ((DexNo.national(265), Map("Level Up (Random < 5) [7]" -> DexNo.national(194), "Level Up (Random > 5) [7]" -> DexNo.national(172))))
		builder += ((DexNo.national(266), Map("Level Up [10]" -> DexNo.national(51))))
		builder += ((DexNo.national(268), Map("Level Up [10]" -> DexNo.national(269))))
		builder += ((DexNo.national(270), Map("Level Up [14]" -> DexNo.national(331))))
		builder += ((DexNo.national(271), Map("Used Item [Water Stone]" -> DexNo.national(295))))
		builder += ((DexNo.national(273), Map("Level Up [14]" -> DexNo.national(363))))
		builder += ((DexNo.national(274), Map("Used Item [Leaf Stone]" -> DexNo.national(186))))
		builder += ((DexNo.national(276), Map("Level Up [22]" -> DexNo.national(5))))
		builder += ((DexNo.national(278), Map("Level Up [25]" -> DexNo.national(211))))
		builder += ((DexNo.national(280), Map("Level Up [20]" -> DexNo.national(374))))
		builder += ((DexNo.national(281), Map("Level Up [30]" -> DexNo.national(229), "Used Item (Male) [Dawn Stone]" -> DexNo.national(282))))
		builder += ((DexNo.national(283), Map("Level Up [22]" -> DexNo.national(53))))
		builder += ((DexNo.national(285), Map("Level Up [23]" -> DexNo.national(286))))
		builder += ((DexNo.national(287), Map("Level Up [18]" -> DexNo.national(310))))
		builder += ((DexNo.national(288), Map("Level Up [36]" -> DexNo.national(445))))
		builder += ((DexNo.national(290), Map("Level Up (Ninjask) [20]" -> DexNo.national(348), "Level Up (Shedinja) [20]" -> DexNo.national(347))))
		builder += ((DexNo.national(293), Map("Level Up [20]" -> DexNo.national(294))))
		builder += ((DexNo.national(294), Map("Level Up [40]" -> DexNo.national(272))))
		builder += ((DexNo.national(296), Map("Level Up [24]" -> DexNo.national(342))))
		builder += ((DexNo.national(298), Map("Level Up with Friendship" -> DexNo.national(39))))
		builder += ((DexNo.national(299), Map("Level Up at Electric" -> DexNo.national(462))))
		builder += ((DexNo.national(300), Map("Used Item [Moon Stone]" -> DexNo.national(325))))
		builder += ((DexNo.national(304), Map("Level Up [32]" -> DexNo.national(288))))
		builder += ((DexNo.national(305), Map("Level Up [42]" -> DexNo.national(245))))
		builder += ((DexNo.national(307), Map("Level Up [37]" -> DexNo.national(24))))
		builder += ((DexNo.national(309), Map("Level Up [26]" -> DexNo.national(310))))
		builder += ((DexNo.national(315), Map("Used Item [Shiny Stone]" -> DexNo.national(157))))
		builder += ((DexNo.national(316), Map("Level Up [26]" -> DexNo.national(317))))
		builder += ((DexNo.national(318), Map("Level Up [30]" -> DexNo.national(319))))
		builder += ((DexNo.national(320), Map("Level Up [40]" -> DexNo.national(317))))
		builder += ((DexNo.national(322), Map("Level Up [33]" -> DexNo.national(119))))
		builder += ((DexNo.national(325), Map("Level Up [32]" -> DexNo.national(326))))
		builder += ((DexNo.national(328), Map("Level Up [35]" -> DexNo.national(252))))
		builder += ((DexNo.national(329), Map("Level Up [45]" -> DexNo.national(254))))
		builder += ((DexNo.national(331), Map("Level Up [32]" -> DexNo.national(272))))
		builder += ((DexNo.national(333), Map("Level Up [35]" -> DexNo.national(334))))
		builder += ((DexNo.national(339), Map("Level Up [30]" -> DexNo.national(80))))
		builder += ((DexNo.national(341), Map("Level Up [30]" -> DexNo.national(297))))
		builder += ((DexNo.national(343), Map("Level Up [36]" -> DexNo.national(344))))
		builder += ((DexNo.national(345), Map("Level Up [40]" -> DexNo.national(335))))
		builder += ((DexNo.national(347), Map("Level Up [40]" -> DexNo.national(348))))
		builder += ((DexNo.national(349), Map("Level Up (Beauty) [170]" -> DexNo.national(334))))
		builder += ((DexNo.national(353), Map("Level Up [37]" -> DexNo.national(354))))
		builder += ((DexNo.national(355), Map("Level Up [37]" -> DexNo.national(210))))
		builder += ((DexNo.national(356), Map("Trade with Held Item [Reaper Cloth]" -> DexNo.national(210))))
		builder += ((DexNo.national(360), Map("Level Up [15]" -> DexNo.national(12))))
		builder += ((DexNo.national(361), Map("Level Up [42]" -> DexNo.national(340), "Used Item (Female) [Dawn Stone]" -> DexNo.national(203))))
		builder += ((DexNo.national(363), Map("Level Up [32]" -> DexNo.national(180))))
		builder += ((DexNo.national(364), Map("Level Up [44]" -> DexNo.national(365))))
		builder += ((DexNo.national(366), Map("Trade with Held Item [Deep Sea Tooth]" -> DexNo.national(409), "Trade with Held Item [Deep Sea Scale]" -> DexNo.national(368))))
		builder += ((DexNo.national(371), Map("Level Up [30]" -> DexNo.national(444))))
		builder += ((DexNo.national(372), Map("Level Up [50]" -> DexNo.national(59))))
		builder += ((DexNo.national(374), Map("Level Up [20]" -> DexNo.national(357))))
		builder += ((DexNo.national(375), Map("Level Up [45]" -> DexNo.national(490))))
		builder += ((DexNo.national(387), Map("Level Up [18]" -> DexNo.national(153))))
		builder += ((DexNo.national(388), Map("Level Up [32]" -> DexNo.national(154))))
		builder += ((DexNo.national(390), Map("Level Up [14]" -> DexNo.national(215))))
		builder += ((DexNo.national(391), Map("Level Up [36]" -> DexNo.national(251))))
		builder += ((DexNo.national(393), Map("Level Up [16]" -> DexNo.national(398))))
		builder += ((DexNo.national(394), Map("Level Up [36]" -> DexNo.national(398))))
		builder += ((DexNo.national(396), Map("Level Up [14]" -> DexNo.national(92))))
		builder += ((DexNo.national(397), Map("Level Up [34]" -> DexNo.national(94))))
		builder += ((DexNo.national(399), Map("Level Up [15]" -> DexNo.national(137))))
		builder += ((DexNo.national(401), Map("Level Up [10]" -> DexNo.national(402))))
		builder += ((DexNo.national(403), Map("Level Up [15]" -> DexNo.national(198))))
		builder += ((DexNo.national(404), Map("Level Up [30]" -> DexNo.national(430))))
		builder += ((DexNo.national(406), Map("Level Up at Morning with Friendship" -> DexNo.national(67))))
		builder += ((DexNo.national(408), Map("Level Up [30]" -> DexNo.national(409))))
		builder += ((DexNo.national(410), Map("Level Up [30]" -> DexNo.national(313))))
		builder += ((DexNo.national(412), Map("Level Up Female [20]" -> DexNo.national(237), "Level Up Male [20]" -> DexNo.national(195))))
		builder += ((DexNo.national(415), Map("Level Up Female [21]" -> DexNo.national(189))))
		builder += ((DexNo.national(418), Map("Level Up [26]" -> DexNo.national(57))))
		builder += ((DexNo.national(420), Map("Level Up [25]" -> DexNo.national(400))))
		builder += ((DexNo.national(422), Map("Level Up [30]" -> DexNo.national(125))))
		builder += ((DexNo.national(425), Map("Level Up [28]" -> DexNo.national(426))))
		builder += ((DexNo.national(427), Map("Level Up with Friendship" -> DexNo.national(135))))
		builder += ((DexNo.national(431), Map("Level Up [38]" -> DexNo.national(432))))
		builder += ((DexNo.national(433), Map("Level Up at Night with Friendship" -> DexNo.national(358))))
		builder += ((DexNo.national(434), Map("Level Up [34]" -> DexNo.national(89))))
		builder += ((DexNo.national(436), Map("Level Up [33]" -> DexNo.national(323))))
		builder += ((DexNo.national(438), Map("Level Up with Move [Mimic]" -> DexNo.national(185))))
		builder += ((DexNo.national(439), Map("Level Up with Move [Mimic]" -> DexNo.national(413))))
		builder += ((DexNo.national(440), Map("Level Up with Held Item (Day) [Oval Stone]" -> DexNo.national(113))))
		builder += ((DexNo.national(443), Map("Level Up [24]" -> DexNo.national(444))))
		builder += ((DexNo.national(444), Map("Level Up [48]" -> DexNo.national(373))))
		builder += ((DexNo.national(446), Map("Level Up with Friendship" -> DexNo.national(473))))
		builder += ((DexNo.national(447), Map("Level Up at Morning with Friendship" -> DexNo.national(260))))
		builder += ((DexNo.national(449), Map("Level Up [34]" -> DexNo.national(142))))
		builder += ((DexNo.national(451), Map("Level Up [40]" -> DexNo.national(234))))
		builder += ((DexNo.national(453), Map("Level Up [37]" -> DexNo.national(38))))
		builder += ((DexNo.national(456), Map("Level Up [31]" -> DexNo.national(409))))
		builder += ((DexNo.national(458), Map("Level Up with Party [Remoraid]" -> DexNo.national(227))))
		builder += ((DexNo.national(459), Map("Level Up [40]" -> DexNo.national(460))))
		
		builder.result
	}

	override def extantDexNos:Seq[DexNo] = DexNo.NationalDexNoRange(1, 493)
}
