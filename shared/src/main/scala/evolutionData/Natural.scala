package com.rayrobdod.possibleEvolutions
package evolutionData

import scala.collection.immutable.Map

object Natural extends SeedData with CompiledOnNoted {
	def game:EvosGame.Value = EvosGame.Natural
	
	lazy val evolutions:Map[DexNo, Map[String, DexNo]] = {
		val builder = Map.newBuilder[DexNo, Map[String, DexNo]]
		
		builder += ((DexNo.national(  1), Map("Level Up [16]" -> DexNo.national(2)) ))
		builder += ((DexNo.national(  2), Map("Level Up [32]" -> DexNo.national(3)) ))
		builder += ((DexNo.national(  4), Map("Level Up [16]" -> DexNo.national(5)) ))
		builder += ((DexNo.national(  5), Map("Level Up [36]" -> DexNo.national(6)) ))
		builder += ((DexNo.national(  7), Map("Level Up [16]" -> DexNo.national(8)) ))
		builder += ((DexNo.national(  8), Map("Level Up [36]" -> DexNo.national(9)) ))
		builder += ((DexNo.national( 10), Map("Level Up [7]" -> DexNo.national(11)) ))
		builder += ((DexNo.national( 11), Map("Level Up [10]" -> DexNo.national(12)) ))
		builder += ((DexNo.national( 13), Map("Level Up [7]" -> DexNo.national(14)) ))
		builder += ((DexNo.national( 14), Map("Level Up [10]" -> DexNo.national(15)) ))
		builder += ((DexNo.national( 16), Map("Level Up [18]" -> DexNo.national(17)) ))
		builder += ((DexNo.national( 17), Map("Level Up [36]" -> DexNo.national(18)) ))
		builder += ((DexNo.national( 19), Map("Level Up [20]" -> DexNo.national(20)) ))
		builder += ((DexNo.alola( 19), Map("Level Up at Night [20]" -> DexNo.alola(20)) ))
		builder += ((DexNo.national( 21), Map("Level Up [20]" -> DexNo.national(22)) ))
		builder += ((DexNo.national( 23), Map("Level Up [22]" -> DexNo.national(24)) ))
		builder += ((DexNo.national( 25), Map("Used Item [Thunder Stone]" -> DexNo.national(26), "Used Item [Thunder Stone] (Alola)" -> DexNo.alola(26)) ))
		builder += ((DexNo.national( 27), Map("Level Up [22]" -> DexNo.national(28)) ))
		builder += ((DexNo.alola( 27), Map("Used Item [Ice Stone]" -> DexNo.alola(28)) ))
		builder += ((DexNo.national( 29), Map("Level Up [16]" -> DexNo.national(30)) ))
		builder += ((DexNo.national( 30), Map("Used Item [Moon Stone]" -> DexNo.national(31)) ))
		builder += ((DexNo.national( 32), Map("Level Up [16]" -> DexNo.national(33)) ))
		builder += ((DexNo.national( 33), Map("Used Item [Moon Stone]" -> DexNo.national(34)) ))
		builder += ((DexNo.national( 35), Map("Used Item [Moon Stone]" -> DexNo.national(36)) ))
		builder += ((DexNo.national( 37), Map("Used Item [Fire Stone]" -> DexNo.national(38)) ))
		builder += ((DexNo.alola( 37), Map("Used Item [Ice Stone]" -> DexNo.alola(38)) ))
		builder += ((DexNo.national( 39), Map("Used Item [Moon Stone]" -> DexNo.national(40)) ))
		builder += ((DexNo.national( 41), Map("Level Up [22]" -> DexNo.national(42)) ))
		builder += ((DexNo.national( 42), Map("Level Up with Friendship" -> DexNo.national(169)) ))
		builder += ((DexNo.national( 43), Map("Level Up [21]" -> DexNo.national(44)) ))
		builder += ((DexNo.national( 44), Map("Used Item [Leaf Stone]" -> DexNo.national(45), "Used Item [Sun Stone]" -> DexNo.national(182)) ))
		builder += ((DexNo.national( 46), Map("Level Up [24]" -> DexNo.national(47)) ))
		builder += ((DexNo.national( 48), Map("Level Up [31]" -> DexNo.national(49)) ))
		builder += ((DexNo.national( 50), Map("Level Up [26]" -> DexNo.national(51)) ))
		builder += ((DexNo.alola( 50), Map("Level Up [26]" -> DexNo.alola(51)) ))
		builder += ((DexNo.national( 52), Map("Level Up [28]" -> DexNo.national(53)) ))
		builder += ((DexNo.alola( 52), Map("Level Up with Friendship" -> DexNo.alola(53)) ))
		builder += ((DexNo.national( 54), Map("Level Up [33]" -> DexNo.national(55)) ))
		builder += ((DexNo.national( 56), Map("Level Up [28]" -> DexNo.national(57)) ))
		builder += ((DexNo.national( 58), Map("Used Item [Fire Stone]" -> DexNo.national(59)) ))
		builder += ((DexNo.national( 60), Map("Level Up [25]" -> DexNo.national(61)) ))
		builder += ((DexNo.national( 61), Map("Used Item [Water Stone]" -> DexNo.national(62), "Trade with Held Item [King's Rock]" -> DexNo.national(186)) ))
		builder += ((DexNo.national( 63), Map("Level Up [16]" -> DexNo.national(64)) ))
		builder += ((DexNo.national( 64), Map("Trade" -> DexNo.national(65)) ))
		builder += ((DexNo.national( 66), Map("Level Up [28]" -> DexNo.national(67)) ))
		builder += ((DexNo.national( 67), Map("Trade" -> DexNo.national(68)) ))
		builder += ((DexNo.national( 69), Map("Level Up [21]" -> DexNo.national(70)) ))
		builder += ((DexNo.national( 70), Map("Used Item [Leaf Stone]" -> DexNo.national(71)) ))
		builder += ((DexNo.national( 72), Map("Level Up [30]" -> DexNo.national(73)) ))
		builder += ((DexNo.national( 74), Map("Level Up [25]" -> DexNo.national(75)) ))
		builder += ((DexNo.national( 75), Map("Trade" -> DexNo.national(76)) ))
		builder += ((DexNo.alola( 74), Map("Level Up [25]" -> DexNo.alola(75)) ))
		builder += ((DexNo.alola( 75), Map("Trade" -> DexNo.alola(76)) ))
		builder += ((DexNo.national( 77), Map("Level Up [40]" -> DexNo.national(78)) ))
		builder += ((DexNo.national( 79), Map("Level Up [37]" -> DexNo.national(80), "Trade with Held Item [King's Rock]" -> DexNo.national(199)) ))
		builder += ((DexNo.national( 81), Map("Level Up [30]" -> DexNo.national(82)) ))
		builder += ((DexNo.national( 82), Map("Level Up at Electric" -> DexNo.national(462)) ))
		builder += ((DexNo.national( 84), Map("Level Up [31]" -> DexNo.national(85)) ))
		builder += ((DexNo.national( 86), Map("Level Up [34]" -> DexNo.national(87)) ))
		builder += ((DexNo.national( 88), Map("Level Up [38]" -> DexNo.national(89)) ))
		builder += ((DexNo.alola( 88), Map("Level Up [38]" -> DexNo.alola(89)) ))
		builder += ((DexNo.national( 90), Map("Used Item [Water Stone]" -> DexNo.national(91)) ))
		builder += ((DexNo.national( 92), Map("Level Up [25]" -> DexNo.national(93)) ))
		builder += ((DexNo.national( 93), Map("Trade" -> DexNo.national(94)) ))
		builder += ((DexNo.national( 95), Map("Trade with Held Item [Metal Coat]" -> DexNo.national(208)) ))
		builder += ((DexNo.national( 96), Map("Level Up [26]" -> DexNo.national(97)) ))
		builder += ((DexNo.national( 98), Map("Level Up [28]" -> DexNo.national(99)) ))
		builder += ((DexNo.national(100), Map("Level Up [30]" -> DexNo.national(101)) ))
		builder += ((DexNo.national(102), Map("Used Item [Leaf Stone]" -> DexNo.national(103), "Used Item [Leaf Stone] (Alola)" -> DexNo.alola(103)) ))
		builder += ((DexNo.national(104), Map("Level Up [28]" -> DexNo.national(105), "Level Up at Night [28] (Alola)" -> DexNo.alola(105)) ))
		builder += ((DexNo.national(108), Map("Level Up with Move [Rollout]" -> DexNo.national(463)) ))
		builder += ((DexNo.national(109), Map("Level Up [35]" -> DexNo.national(110)) ))
		builder += ((DexNo.national(111), Map("Level Up [42]" -> DexNo.national(112)) ))
		builder += ((DexNo.national(112), Map("Trade with Held Item [Protector]" -> DexNo.national(464)) ))
		builder += ((DexNo.national(113), Map("Level Up with Friendship" -> DexNo.national(242)) ))
		builder += ((DexNo.national(114), Map("Level Up with Move [Ancient Power]" -> DexNo.national(465)) ))
		builder += ((DexNo.national(116), Map("Level Up [32]" -> DexNo.national(117)) ))
		builder += ((DexNo.national(117), Map("Trade with Held Item [Dragon Scale]" -> DexNo.national(230)) ))
		builder += ((DexNo.national(118), Map("Level Up [33]" -> DexNo.national(119)) ))
		builder += ((DexNo.national(120), Map("Used Item [Water Stone]" -> DexNo.national(121)) ))
		builder += ((DexNo.national(123), Map("Trade with Held Item [Metal Coat]" -> DexNo.national(212)) ))
		builder += ((DexNo.national(125), Map("Trade with Held Item [Electirizer]" -> DexNo.national(466)) ))
		builder += ((DexNo.national(126), Map("Trade with Held Item [Magmarizer]" -> DexNo.national(467)) ))
		builder += ((DexNo.national(129), Map("Level Up [20]" -> DexNo.national(130)) ))
		builder += ((DexNo.national(133), Map(
				"Used Item [Water Stone]" -> DexNo.national(134), "Used Item [Thunder Stone]" -> DexNo.national(135), "Used Item [Fire Stone]" -> DexNo.national(136),
				"Level Up at Morning" -> DexNo.national(196), "Level Up at Night" -> DexNo.national(197),
				"Level Up at Forest" -> DexNo.national(470), "Level Up at Cold" -> DexNo.national(471),
				"Level Up with 50 Affection + MoveType [Fairy]" -> DexNo.national(700)
		) ))
		builder += ((DexNo.national(137), Map("Trade with Held Item [Up-Grade]" -> DexNo.national(233)) ))
		builder += ((DexNo.national(138), Map("Level Up [40]" -> DexNo.national(139)) ))
		builder += ((DexNo.national(140), Map("Level Up [40]" -> DexNo.national(141)) ))
		builder += ((DexNo.national(147), Map("Level Up [30]" -> DexNo.national(148)) ))
		builder += ((DexNo.national(148), Map("Level Up [55]" -> DexNo.national(149)) ))
		builder += ((DexNo.national(152), Map("Level Up [16]" -> DexNo.national(153)) ))
		builder += ((DexNo.national(153), Map("Level Up [32]" -> DexNo.national(154)) ))
		builder += ((DexNo.national(155), Map("Level Up [14]" -> DexNo.national(156)) ))
		builder += ((DexNo.national(156), Map("Level Up [36]" -> DexNo.national(157)) ))
		builder += ((DexNo.national(158), Map("Level Up [18]" -> DexNo.national(159)) ))
		builder += ((DexNo.national(159), Map("Level Up [30]" -> DexNo.national(160)) ))
		builder += ((DexNo.national(161), Map("Level Up [15]" -> DexNo.national(162)) ))
		builder += ((DexNo.national(163), Map("Level Up [20]" -> DexNo.national(164)) ))
		builder += ((DexNo.national(165), Map("Level Up [18]" -> DexNo.national(166)) ))
		builder += ((DexNo.national(167), Map("Level Up [22]" -> DexNo.national(168)) ))
		builder += ((DexNo.national(170), Map("Level Up [27]" -> DexNo.national(171)) ))
		builder += ((DexNo.national(172), Map("Level Up with Friendship" -> DexNo.national(25)) ))
		builder += ((DexNo.national(173), Map("Level Up with Friendship" -> DexNo.national(35)) ))
		builder += ((DexNo.national(174), Map("Level Up with Friendship" -> DexNo.national(39)) ))
		builder += ((DexNo.national(175), Map("Level Up with Friendship" -> DexNo.national(176)) ))
		builder += ((DexNo.national(176), Map("Used Item [Shiny Stone]" -> DexNo.national(468)) ))
		builder += ((DexNo.national(177), Map("Level Up [25]" -> DexNo.national(178)) ))
		builder += ((DexNo.national(179), Map("Level Up [15]" -> DexNo.national(180)) ))
		builder += ((DexNo.national(180), Map("Level Up [30]" -> DexNo.national(181)) ))
		builder += ((DexNo.national(183), Map("Level Up [18]" -> DexNo.national(184)) ))
		builder += ((DexNo.national(187), Map("Level Up [18]" -> DexNo.national(188)) ))
		builder += ((DexNo.national(188), Map("Level Up [27]" -> DexNo.national(189)) ))
		builder += ((DexNo.national(190), Map("Level Up with Move [Double Hit]" -> DexNo.national(424)) ))
		builder += ((DexNo.national(191), Map("Used Item [Sun Stone]" -> DexNo.national(192)) ))
		builder += ((DexNo.national(193), Map("Level Up with Move [Ancient Power]" -> DexNo.national(469)) ))
		builder += ((DexNo.national(194), Map("Level Up [20]" -> DexNo.national(195)) ))
		builder += ((DexNo.national(198), Map("Used Item [Dusk Stone]" -> DexNo.national(430)) ))
		builder += ((DexNo.national(200), Map("Used Item [Dusk Stone]" -> DexNo.national(429)) ))
		builder += ((DexNo.national(204), Map("Level Up [31]" -> DexNo.national(205)) ))
		builder += ((DexNo.national(207), Map("Level Up with Held Item (Night) [Razor Fang]" -> DexNo.national(472)) ))
		builder += ((DexNo.national(209), Map("Level Up [23]" -> DexNo.national(210)) ))
		builder += ((DexNo.national(215), Map("Level Up with Held Item (Night) [Razor Claw]" -> DexNo.national(461)) ))
		builder += ((DexNo.national(216), Map("Level Up [30]" -> DexNo.national(217)) ))
		builder += ((DexNo.national(218), Map("Level Up [38]" -> DexNo.national(219)) ))
		builder += ((DexNo.national(220), Map("Level Up [33]" -> DexNo.national(221)) ))
		builder += ((DexNo.national(221), Map("Level Up with Move [Ancient Power]" -> DexNo.national(473)) ))
		builder += ((DexNo.national(223), Map("Level Up [25]" -> DexNo.national(224)) ))
		builder += ((DexNo.national(228), Map("Level Up [24]" -> DexNo.national(229)) ))
		builder += ((DexNo.national(231), Map("Level Up [25]" -> DexNo.national(232)) ))
		builder += ((DexNo.national(233), Map("Trade with Held Item [Dubious Disc]" -> DexNo.national(474)) ))
		builder += ((DexNo.national(236), Map("Level Up (Attack > Defense) [20]" -> DexNo.national(106), "Level Up (Attack < Defense) [20]" -> DexNo.national(107), "Level Up (Attack = Defense) [20]" -> DexNo.national(237)) ))
		builder += ((DexNo.national(238), Map("Level Up [30]" -> DexNo.national(124)) ))
		builder += ((DexNo.national(239), Map("Level Up [30]" -> DexNo.national(125)) ))
		builder += ((DexNo.national(240), Map("Level Up [30]" -> DexNo.national(126)) ))
		builder += ((DexNo.national(246), Map("Level Up [30]" -> DexNo.national(247)) ))
		builder += ((DexNo.national(247), Map("Level Up [55]" -> DexNo.national(248)) ))
		builder += ((DexNo.national(252), Map("Level Up [16]" -> DexNo.national(253)) ))
		builder += ((DexNo.national(253), Map("Level Up [36]" -> DexNo.national(254)) ))
		builder += ((DexNo.national(255), Map("Level Up [16]" -> DexNo.national(256)) ))
		builder += ((DexNo.national(256), Map("Level Up [36]" -> DexNo.national(257)) ))
		builder += ((DexNo.national(258), Map("Level Up [16]" -> DexNo.national(259)) ))
		builder += ((DexNo.national(259), Map("Level Up [36]" -> DexNo.national(260)) ))
		builder += ((DexNo.national(261), Map("Level Up [18]" -> DexNo.national(262)) ))
		builder += ((DexNo.national(263), Map("Level Up [20]" -> DexNo.national(264)) ))
		builder += ((DexNo.national(265), Map("Level Up (Random < 5) [7]" -> DexNo.national(266), "Level Up (Random > 5) [7]" -> DexNo.national(268)) ))
		builder += ((DexNo.national(266), Map("Level Up [10]" -> DexNo.national(267)) ))
		builder += ((DexNo.national(268), Map("Level Up [10]" -> DexNo.national(269)) ))
		builder += ((DexNo.national(270), Map("Level Up [14]" -> DexNo.national(271)) ))
		builder += ((DexNo.national(271), Map("Used Item [Water Stone]" -> DexNo.national(272)) ))
		builder += ((DexNo.national(273), Map("Level Up [14]" -> DexNo.national(274)) ))
		builder += ((DexNo.national(274), Map("Used Item [Leaf Stone]" -> DexNo.national(275)) ))
		builder += ((DexNo.national(276), Map("Level Up [22]" -> DexNo.national(277)) ))
		builder += ((DexNo.national(278), Map("Level Up [25]" -> DexNo.national(279)) ))
		builder += ((DexNo.national(280), Map("Level Up [20]" -> DexNo.national(281)) ))
		builder += ((DexNo.national(281), Map("Level Up [30]" -> DexNo.national(282), "Level Up with Held Item (Male) [Dawn Stone]" -> DexNo.national(475)) ))
		builder += ((DexNo.national(283), Map("Level Up [22]" -> DexNo.national(284)) ))
		builder += ((DexNo.national(285), Map("Level Up [23]" -> DexNo.national(286)) ))
		builder += ((DexNo.national(287), Map("Level Up [18]" -> DexNo.national(288)) ))
		builder += ((DexNo.national(288), Map("Level Up [36]" -> DexNo.national(289)) ))
		builder += ((DexNo.national(290), Map("Level Up (Ninjask) [20]" -> DexNo.national(291), "Level Up (Shedinja) [20]" -> DexNo.national(292)) ))
		builder += ((DexNo.national(293), Map("Level Up [20]" -> DexNo.national(294)) ))
		builder += ((DexNo.national(294), Map("Level Up [40]" -> DexNo.national(295)) ))
		builder += ((DexNo.national(296), Map("Level Up [24]" -> DexNo.national(297)) ))
		builder += ((DexNo.national(298), Map("Level Up with Friendship" -> DexNo.national(183)) ))
		builder += ((DexNo.national(299), Map("Level Up at Electric" -> DexNo.national(476)) ))
		builder += ((DexNo.national(300), Map("Used Item [Moon Stone]" -> DexNo.national(301)) ))
		builder += ((DexNo.national(304), Map("Level Up [32]" -> DexNo.national(305)) ))
		builder += ((DexNo.national(305), Map("Level Up [42]" -> DexNo.national(306)) ))
		builder += ((DexNo.national(307), Map("Level Up [37]" -> DexNo.national(308)) ))
		builder += ((DexNo.national(309), Map("Level Up [26]" -> DexNo.national(310)) ))
		builder += ((DexNo.national(315), Map("Used Item [Shiny Stone]" -> DexNo.national(407)) ))
		builder += ((DexNo.national(316), Map("Level Up [26]" -> DexNo.national(317)) ))
		builder += ((DexNo.national(318), Map("Level Up [30]" -> DexNo.national(319)) ))
		builder += ((DexNo.national(320), Map("Level Up [40]" -> DexNo.national(321)) ))
		builder += ((DexNo.national(322), Map("Level Up [33]" -> DexNo.national(323)) ))
		builder += ((DexNo.national(325), Map("Level Up [32]" -> DexNo.national(326)) ))
		builder += ((DexNo.national(328), Map("Level Up [35]" -> DexNo.national(329)) ))
		builder += ((DexNo.national(329), Map("Level Up [45]" -> DexNo.national(330)) ))
		builder += ((DexNo.national(331), Map("Level Up [32]" -> DexNo.national(332)) ))
		builder += ((DexNo.national(333), Map("Level Up [35]" -> DexNo.national(334)) ))
		builder += ((DexNo.national(339), Map("Level Up [30]" -> DexNo.national(340)) ))
		builder += ((DexNo.national(341), Map("Level Up [30]" -> DexNo.national(342)) ))
		builder += ((DexNo.national(343), Map("Level Up [36]" -> DexNo.national(344)) ))
		builder += ((DexNo.national(345), Map("Level Up [40]" -> DexNo.national(346)) ))
		builder += ((DexNo.national(347), Map("Level Up [40]" -> DexNo.national(348)) ))
		builder += ((DexNo.national(349), Map("Level Up (Beauty) [170]" -> DexNo.national(350), "Trade with Held Item [Prism Scale]" -> DexNo.national(350)) ))
		builder += ((DexNo.national(353), Map("Level Up [37]" -> DexNo.national(354)) ))
		builder += ((DexNo.national(355), Map("Level Up [37]" -> DexNo.national(356)) ))
		builder += ((DexNo.national(356), Map("Trade with Held Item [Reaper Cloth]" -> DexNo.national(477)) ))
		builder += ((DexNo.national(360), Map("Level Up [15]" -> DexNo.national(202)) ))
		builder += ((DexNo.national(361), Map("Level Up [42]" -> DexNo.national(362), "Level Up with Held Item (Female) [Dawn Stone]" -> DexNo.national(478)) ))
		builder += ((DexNo.national(363), Map("Level Up [32]" -> DexNo.national(364)) ))
		builder += ((DexNo.national(364), Map("Level Up [44]" -> DexNo.national(365)) ))
		builder += ((DexNo.national(366), Map("Trade with Held Item [Deep Sea Tooth]" -> DexNo.national(367), "Trade with Held Item [Deep Sea Scale]" -> DexNo.national(368)) ))
		builder += ((DexNo.national(371), Map("Level Up [30]" -> DexNo.national(372)) ))
		builder += ((DexNo.national(372), Map("Level Up [50]" -> DexNo.national(373)) ))
		builder += ((DexNo.national(374), Map("Level Up [20]" -> DexNo.national(375)) ))
		builder += ((DexNo.national(375), Map("Level Up [45]" -> DexNo.national(376)) ))
		builder += ((DexNo.national(387), Map("Level Up [18]" -> DexNo.national(388)) ))
		builder += ((DexNo.national(388), Map("Level Up [32]" -> DexNo.national(389)) ))
		builder += ((DexNo.national(390), Map("Level Up [14]" -> DexNo.national(391)) ))
		builder += ((DexNo.national(391), Map("Level Up [36]" -> DexNo.national(392)) ))
		builder += ((DexNo.national(393), Map("Level Up [16]" -> DexNo.national(394)) ))
		builder += ((DexNo.national(394), Map("Level Up [36]" -> DexNo.national(395)) ))
		builder += ((DexNo.national(396), Map("Level Up [14]" -> DexNo.national(397)) ))
		builder += ((DexNo.national(397), Map("Level Up [34]" -> DexNo.national(398)) ))
		builder += ((DexNo.national(399), Map("Level Up [15]" -> DexNo.national(400)) ))
		builder += ((DexNo.national(401), Map("Level Up [10]" -> DexNo.national(402)) ))
		builder += ((DexNo.national(403), Map("Level Up [15]" -> DexNo.national(404)) ))
		builder += ((DexNo.national(404), Map("Level Up [30]" -> DexNo.national(405)) ))
		builder += ((DexNo.national(406), Map("Level Up at Morning" -> DexNo.national(315)) ))
		builder += ((DexNo.national(408), Map("Level Up [30]" -> DexNo.national(409)) ))
		builder += ((DexNo.national(410), Map("Level Up [30]" -> DexNo.national(411)) ))
		builder += ((DexNo.national(412), Map("Level Up Female [20]" -> DexNo.national(413), "Level Up Male [20]" -> DexNo.national(414)) ))
		builder += ((DexNo.national(415), Map("Level Up Female [21]" -> DexNo.national(416)) ))
		builder += ((DexNo.national(418), Map("Level Up [26]" -> DexNo.national(419)) ))
		builder += ((DexNo.national(420), Map("Level Up [25]" -> DexNo.national(421)) ))
		builder += ((DexNo.national(422), Map("Level Up [30]" -> DexNo.national(423)) ))
		builder += ((DexNo.national(425), Map("Level Up [28]" -> DexNo.national(426)) ))
		builder += ((DexNo.national(427), Map("Level Up with Friendship" -> DexNo.national(428)) ))
		builder += ((DexNo.national(431), Map("Level Up [38]" -> DexNo.national(432)) ))
		builder += ((DexNo.national(433), Map("Level Up at Night" -> DexNo.national(358)) ))
		builder += ((DexNo.national(434), Map("Level Up [34]" -> DexNo.national(435)) ))
		builder += ((DexNo.national(436), Map("Level Up [33]" -> DexNo.national(437)) ))
		builder += ((DexNo.national(438), Map("Level Up with Move [Mimic]" -> DexNo.national(185)) ))
		builder += ((DexNo.national(439), Map("Level Up with Move [Mimic]" -> DexNo.national(122)) ))
		builder += ((DexNo.national(440), Map("Level Up with Held Item (Day) [Oval Stone]" -> DexNo.national(113)) ))
		builder += ((DexNo.national(443), Map("Level Up [24]" -> DexNo.national(444)) ))
		builder += ((DexNo.national(444), Map("Level Up [48]" -> DexNo.national(445)) ))
		builder += ((DexNo.national(446), Map("Level Up with Friendship" -> DexNo.national(143)) ))
		builder += ((DexNo.national(447), Map("Level Up at Morning" -> DexNo.national(448)) ))
		builder += ((DexNo.national(449), Map("Level Up [34]" -> DexNo.national(450)) ))
		builder += ((DexNo.national(451), Map("Level Up [40]" -> DexNo.national(452)) ))
		builder += ((DexNo.national(453), Map("Level Up [37]" -> DexNo.national(454)) ))
		builder += ((DexNo.national(456), Map("Level Up [31]" -> DexNo.national(457)) ))
		builder += ((DexNo.national(458), Map("Level up with Party [Remoraid]" -> DexNo.national(226)) ))
		builder += ((DexNo.national(459), Map("Level Up [40]" -> DexNo.national(460)) ))
		builder += ((DexNo.national(495), Map("Level Up [17]" -> DexNo.national(496)) ))
		builder += ((DexNo.national(496), Map("Level Up [36]" -> DexNo.national(497)) ))
		builder += ((DexNo.national(498), Map("Level Up [17]" -> DexNo.national(499)) ))
		builder += ((DexNo.national(499), Map("Level Up [36]" -> DexNo.national(500)) ))
		builder += ((DexNo.national(501), Map("Level Up [17]" -> DexNo.national(502)) ))
		builder += ((DexNo.national(502), Map("Level Up [36]" -> DexNo.national(503)) ))
		builder += ((DexNo.national(504), Map("Level Up [20]" -> DexNo.national(505)) ))
		builder += ((DexNo.national(506), Map("Level Up [16]" -> DexNo.national(507)) ))
		builder += ((DexNo.national(507), Map("Level Up [32]" -> DexNo.national(508)) ))
		builder += ((DexNo.national(509), Map("Level Up [20]" -> DexNo.national(510)) ))
		builder += ((DexNo.national(511), Map("Used Item [Leaf Stone]" -> DexNo.national(512)) ))
		builder += ((DexNo.national(513), Map("Used Item [Fire Stone]" -> DexNo.national(514)) ))
		builder += ((DexNo.national(515), Map("Used Item [Water Stone]" -> DexNo.national(516)) ))
		builder += ((DexNo.national(517), Map("Used Item [Moon Stone]" -> DexNo.national(518)) ))
		builder += ((DexNo.national(519), Map("Level Up [21]" -> DexNo.national(520)) ))
		builder += ((DexNo.national(520), Map("Level Up [32]" -> DexNo.national(521)) ))
		builder += ((DexNo.national(522), Map("Level Up [27]" -> DexNo.national(523)) ))
		builder += ((DexNo.national(524), Map("Level Up [25]" -> DexNo.national(525)) ))
		builder += ((DexNo.national(525), Map("Trade" -> DexNo.national(526)) ))
		builder += ((DexNo.national(527), Map("Level Up with Friendship" -> DexNo.national(528)) ))
		builder += ((DexNo.national(529), Map("Level Up [31]" -> DexNo.national(530)) ))
		builder += ((DexNo.national(532), Map("Level Up [25]" -> DexNo.national(533)) ))
		builder += ((DexNo.national(533), Map("Trade" -> DexNo.national(534)) ))
		builder += ((DexNo.national(535), Map("Level Up [25]" -> DexNo.national(536)) ))
		builder += ((DexNo.national(536), Map("Level Up [36]" -> DexNo.national(537)) ))
		builder += ((DexNo.national(540), Map("Level Up [20]" -> DexNo.national(541)) ))
		builder += ((DexNo.national(541), Map("Level Up with Friendship" -> DexNo.national(542)) ))
		builder += ((DexNo.national(543), Map("Level Up [22]" -> DexNo.national(544)) ))
		builder += ((DexNo.national(544), Map("Level Up [30]" -> DexNo.national(545)) ))
		builder += ((DexNo.national(546), Map("Used Item [Sun Stone]" -> DexNo.national(547)) ))
		builder += ((DexNo.national(548), Map("Used Item [Sun Stone]" -> DexNo.national(549)) ))
		builder += ((DexNo.national(551), Map("Level Up [29]" -> DexNo.national(552)) ))
		builder += ((DexNo.national(552), Map("Level Up [40]" -> DexNo.national(553)) ))
		builder += ((DexNo.national(554), Map("Level Up [35]" -> DexNo.national(555)) ))
		builder += ((DexNo.national(557), Map("Level Up [34]" -> DexNo.national(558)) ))
		builder += ((DexNo.national(559), Map("Level Up [39]" -> DexNo.national(560)) ))
		builder += ((DexNo.national(562), Map("Level Up [34]" -> DexNo.national(563)) ))
		builder += ((DexNo.national(564), Map("Level Up [37]" -> DexNo.national(565)) ))
		builder += ((DexNo.national(566), Map("Level Up [37]" -> DexNo.national(567)) ))
		builder += ((DexNo.national(568), Map("Level Up [36]" -> DexNo.national(569)) ))
		builder += ((DexNo.national(570), Map("Level Up [30]" -> DexNo.national(571)) ))
		builder += ((DexNo.national(572), Map("Used Item [Shiny Stone]" -> DexNo.national(573)) ))
		builder += ((DexNo.national(574), Map("Level Up [32]" -> DexNo.national(575)) ))
		builder += ((DexNo.national(575), Map("Level Up [41]" -> DexNo.national(576)) ))
		builder += ((DexNo.national(577), Map("Level Up [32]" -> DexNo.national(578)) ))
		builder += ((DexNo.national(578), Map("Level Up [41]" -> DexNo.national(579)) ))
		builder += ((DexNo.national(580), Map("Level Up [35]" -> DexNo.national(581)) ))
		builder += ((DexNo.national(582), Map("Level Up [35]" -> DexNo.national(583)) ))
		builder += ((DexNo.national(583), Map("Level Up [47]" -> DexNo.national(584)) ))
		builder += ((DexNo.national(585), Map("Level Up [34]" -> DexNo.national(586)) ))
		builder += ((DexNo.national(588), Map("Trade for opposite Karrablast/Shelmet" -> DexNo.national(589)) ))
		builder += ((DexNo.national(590), Map("Level Up [39]" -> DexNo.national(591)) ))
		builder += ((DexNo.national(592), Map("Level Up [40]" -> DexNo.national(593)) ))
		builder += ((DexNo.national(595), Map("Level Up [36]" -> DexNo.national(596)) ))
		builder += ((DexNo.national(597), Map("Level Up [40]" -> DexNo.national(598)) ))
		builder += ((DexNo.national(599), Map("Level Up [38]" -> DexNo.national(600)) ))
		builder += ((DexNo.national(600), Map("Level Up [49]" -> DexNo.national(601)) ))
		builder += ((DexNo.national(602), Map("Level Up [39]" -> DexNo.national(603)) ))
		builder += ((DexNo.national(603), Map("Used Item [Thunder Stone]" -> DexNo.national(604)) ))
		builder += ((DexNo.national(605), Map("Level Up [42]" -> DexNo.national(606)) ))
		builder += ((DexNo.national(607), Map("Level Up [41]" -> DexNo.national(608)) ))
		builder += ((DexNo.national(608), Map("Used Item [Dusk Stone]" -> DexNo.national(609)) ))
		builder += ((DexNo.national(610), Map("Level Up [38]" -> DexNo.national(611)) ))
		builder += ((DexNo.national(611), Map("Level Up [48]" -> DexNo.national(612)) ))
		builder += ((DexNo.national(613), Map("Level Up [37]" -> DexNo.national(614)) ))
		builder += ((DexNo.national(616), Map("Trade for opposite Karrablast/Shelmet" -> DexNo.national(617)) ))
		builder += ((DexNo.national(619), Map("Level Up [50]" -> DexNo.national(620)) ))
		builder += ((DexNo.national(622), Map("Level Up [43]" -> DexNo.national(623)) ))
		builder += ((DexNo.national(624), Map("Level Up [52]" -> DexNo.national(625)) ))
		builder += ((DexNo.national(627), Map("Level Up [54]" -> DexNo.national(628)) ))
		builder += ((DexNo.national(629), Map("Level Up [54]" -> DexNo.national(630)) ))
		builder += ((DexNo.national(633), Map("Level Up [50]" -> DexNo.national(634)) ))
		builder += ((DexNo.national(634), Map("Level Up [64]" -> DexNo.national(635)) ))
		builder += ((DexNo.national(636), Map("Level Up [59]" -> DexNo.national(637)) ))
		builder += ((DexNo.national(650), Map("Level Up [16]" -> DexNo.national(651)) ))
		builder += ((DexNo.national(651), Map("Level Up [36]" -> DexNo.national(652)) ))
		builder += ((DexNo.national(653), Map("Level Up [16]" -> DexNo.national(654)) ))
		builder += ((DexNo.national(654), Map("Level Up [36]" -> DexNo.national(655)) ))
		builder += ((DexNo.national(656), Map("Level Up [16]" -> DexNo.national(657)) ))
		builder += ((DexNo.national(657), Map("Level Up [36]" -> DexNo.national(658)) ))
		builder += ((DexNo.national(659), Map("Level Up [20]" -> DexNo.national(660)) ))
		builder += ((DexNo.national(661), Map("Level Up [17]" -> DexNo.national(662)) ))
		builder += ((DexNo.national(662), Map("Level Up [35]" -> DexNo.national(663)) ))
		builder += ((DexNo.national(664), Map("Level Up [9]" -> DexNo.national(665)) ))
		builder += ((DexNo.national(665), Map("Level Up [12]" -> DexNo.national(666)) ))
		builder += ((DexNo.national(667), Map("Level Up [35]" -> DexNo.national(668)) ))
		builder += ((DexNo.national(669), Map("Level Up [19]" -> DexNo.national(670)) ))
		builder += ((DexNo.national(670), Map("Used Item [Shiny Stone]" -> DexNo.national(671)) ))
		builder += ((DexNo.national(672), Map("Level Up [32]" -> DexNo.national(673)) ))
		builder += ((DexNo.national(674), Map("Dark Type in Party [32]" -> DexNo.national(675)) ))
		builder += ((DexNo.national(677), Map("Level Up Male [25]" -> DexNo.national(678), "Level Up Female (SetForm 1) [25]" -> DexNo.national(678)) ))
		builder += ((DexNo.national(679), Map("Level Up [35]" -> DexNo.national(680)) ))
		builder += ((DexNo.national(680), Map("Used Item [Dusk Stone]" -> DexNo.national(681)) ))
		builder += ((DexNo.national(682), Map("Trade with Held Item [Sachet]" -> DexNo.national(683)) ))
		builder += ((DexNo.national(684), Map("Trade with Held Item [Whipped Dream]" -> DexNo.national(685)) ))
		builder += ((DexNo.national(686), Map("Level Up with 3DS Upside Down [30]" -> DexNo.national(687)) ))
		builder += ((DexNo.national(688), Map("Level Up [39]" -> DexNo.national(689)) ))
		builder += ((DexNo.national(690), Map("Level Up [48]" -> DexNo.national(691)) ))
		builder += ((DexNo.national(692), Map("Level Up [37]" -> DexNo.national(693)) ))
		builder += ((DexNo.national(694), Map("Used Item [Sun Stone]" -> DexNo.national(695)) ))
		builder += ((DexNo.national(696), Map("Level Up at Night [39]" -> DexNo.national(697)) ))
		builder += ((DexNo.national(698), Map("Level Up at Night [39]" -> DexNo.national(699)) ))
		builder += ((DexNo.national(704), Map("Level Up [40]" -> DexNo.national(705)) ))
		builder += ((DexNo.national(705), Map("Overworld Rain [50]" -> DexNo.national(706)) ))
		builder += ((DexNo.national(708), Map("Trade" -> DexNo.national(709)) ))
		builder += ((DexNo.national(710), Map("Trade" -> DexNo.national(711)) ))
		builder += ((DexNo.national(712), Map("Level Up [37]" -> DexNo.national(713)) ))
		builder += ((DexNo.national(714), Map("Level Up [48]" -> DexNo.national(715)) ))
		builder += ((DexNo.national(722), Map("Level Up [17]" -> DexNo.national(723)) ))
		builder += ((DexNo.national(723), Map("Level Up [34]" -> DexNo.national(724)) ))
		builder += ((DexNo.national(725), Map("Level Up [17]" -> DexNo.national(726)) ))
		builder += ((DexNo.national(726), Map("Level Up [34]" -> DexNo.national(727)) ))
		builder += ((DexNo.national(728), Map("Level Up [17]" -> DexNo.national(729)) ))
		builder += ((DexNo.national(729), Map("Level Up [34]" -> DexNo.national(730)) ))
		builder += ((DexNo.national(731), Map("Level Up [14]" -> DexNo.national(732)) ))
		builder += ((DexNo.national(732), Map("Level Up [28]" -> DexNo.national(733)) ))
		builder += ((DexNo.national(734), Map("Level Up at Morning [20]" -> DexNo.national(735)) ))
		builder += ((DexNo.national(736), Map("Level Up [20]" -> DexNo.national(737)) ))
		builder += ((DexNo.national(737), Map("Level Up at Electric" -> DexNo.national(738)) ))
		builder += ((DexNo.national(739), Map("Level Up Summit" -> DexNo.national(740)) ))
		builder += ((DexNo.national(742), Map("Level Up [25]" -> DexNo.national(743)) ))
		builder += ((DexNo.national(744), Map("Level Up at Morning in Sun [25]" -> DexNo.national(745), "Level Up at Night in Moon (SetForm 1) [25]" -> DexNo.national(745)) ))
		builder += ((DexNo.duskRockruff, Map("Level Up at Dusk [25]" -> DexNo.national(745)) ))
		builder += ((DexNo.national(747), Map("Level Up [38]" -> DexNo.national(748)) ))
		builder += ((DexNo.national(749), Map("Level Up [30]" -> DexNo.national(750)) ))
		builder += ((DexNo.national(751), Map("Level Up [22]" -> DexNo.national(752)) ))
		builder += ((DexNo.national(753), Map("Level Up at Morning [34]" -> DexNo.national(754)) ))
		builder += ((DexNo.national(755), Map("Level Up [24]" -> DexNo.national(756)) ))
		builder += ((DexNo.national(757), Map("Level Up Female [33]" -> DexNo.national(758)) ))
		builder += ((DexNo.national(759), Map("Level Up [27]" -> DexNo.national(760)) ))
		builder += ((DexNo.national(761), Map("Level Up [18]" -> DexNo.national(762)) ))
		builder += ((DexNo.national(762), Map("Level Up with Move [Stomp]" -> DexNo.national(763)) ))
		builder += ((DexNo.national(767), Map("Level Up [30]" -> DexNo.national(768)) ))
		builder += ((DexNo.national(769), Map("Level Up [42]" -> DexNo.national(770)) ))
		builder += ((DexNo.national(772), Map("Level Up with Friendship" -> DexNo.national(773)) ))
		builder += ((DexNo.national(782), Map("Level Up [35]" -> DexNo.national(783)) ))
		builder += ((DexNo.national(783), Map("Level Up [45]" -> DexNo.national(784)) ))
		builder += ((DexNo.national(789), Map("Level Up [43]" -> DexNo.national(790)) ))
		builder += ((DexNo.national(790), Map("Level Up in Sun [53]" -> DexNo.national(791), "Level Up in Moon [53]" -> DexNo.national(792)) ))
		builder += ((DexNo.national(803), Map("Level Up with Move [Dragon Pulse]" -> DexNo.national(804)) ))
		
		builder.result
	}
	
	val compiledOn:java.time.Instant = compiledOnMacro.apply
}