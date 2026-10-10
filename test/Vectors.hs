{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Vectors
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Test vectors: real mainnet gossip, and the extended gossip query
-- vectors of BOLT #7 (bolt07/extended-queries.json). Hex strings.

module Vectors (
    AnnVector(..)
  , UpdateVector(..)
  , mainnet_announcements
  , mainnet_updates
  , node_announcement_vector
  , query_vectors
  ) where

import qualified Data.ByteString as BS
import Data.Word (Word32)

-- mainnet gossip -------------------------------------------------------------

-- Real mainnet channel_announcements and channel_updates, as captured
-- from a peer. Payloads exclude the 2-byte message type. The digests
-- (double SHA-256 of the signed bytes) and CRC-32C checksums were
-- computed independently of this library.

-- | A channel_announcement payload and its signature digest.
data AnnVector = AnnVector
  { av_payload :: !BS.ByteString  -- ^ hex
  , av_digest  :: !BS.ByteString  -- ^ hex
  }

-- | A channel_update payload, its signature digest and checksum, and
--   the index of its channel's announcement in 'mainnet_announcements'.
data UpdateVector = UpdateVector
  { uv_payload  :: !BS.ByteString  -- ^ hex
  , uv_digest   :: !BS.ByteString  -- ^ hex
  , uv_checksum :: !Word32
  , uv_ann      :: !Int
  }

mainnet_announcements :: [AnnVector]
mainnet_announcements = [
    -- 850052x2992x1
    AnnVector (mconcat
        [ "3c256c270126d2c52ab5464fa959d627e9baec8c76bd2b81d34b47c4efb03290"
        , "40126ea7a0f8d31420976dc28ff684dc63f62620fa10dbac4fe3d417ca5690c1"
        , "b7fe1de61f5dd2b8368886d6c98f625c9110198e57c14fac93d4eff34e19ce5f"
        , "5dc6fef4501e8105c8f9b82d6d0b348f5840864445b071ae7a741b5eb92d2ec0"
        , "ab85681e4e1e85144bbbd8757c14cee92468d2c5ac84345cb28931b6fed5e339"
        , "00ed9dd363db630f31248cfa933a3667cbf87bb78a48a442e81c652686355365"
        , "ff33164c7588f0a753d50c4d52dd4a81c611fa47b4e950dfc9e3379a12ef7834"
        , "781c0900b90f56a5a49893446d03c8d283c3c093e3c8c682ff6cb40d70d9eebc"
        , "00006fe28c0ab6f1b372c1a6a246ae63f74f931e8365e15a089c68d619000000"
        , "00000cf884000bb0000102bb5e47ee26aca1654ce092d9d06eac15d953210c73"
        , "7f2220e2d7e54c4f108677030b828fd1e42549fbc2f1d0960ec3663362c41777"
        , "d3d87fe1b8e1d4bf063abcaa02bc3f260955062a4f427704edef1864c76d6777"
        , "fe81bed43a5f6297dda1c7ca9203523e164f4f9fbdea978c6570d56b13ca8338"
        , "430e6794ff74cf6aa0dbda71dd2d"
        ])
      "3dc0e5a431551c44f2afd49cda2d3bf2a5f1b9cc86ef93db8953c17dd1ef4ded"
    -- 850013x1067x1
  , AnnVector (mconcat
        [ "f37bf5c2f2c11c79541fe16c2a5d63b8795d583ba72f8f1ec9160bbe02a7101d"
        , "519aa61c125737f82a67be9a586da3e04f941907d9a9c59dd77dd2ae965df2cd"
        , "f3e247f3f0227ef24baa2ede50f1844e12db674ac4ea53644d701521951ab3c5"
        , "49eb93a75d6984cb5ca7a5872f0a27f6aa3608a5dfe4468e3f941af8c86425d3"
        , "4c1ac5663d72b2934d74b20a2f364f3cd09c6528d47e09f9be99be54b1338b0e"
        , "6bf9ee1bad5bf92cc95de7b0d268d92e9c671f2cfe51a0f5edbf93bf7ee9232e"
        , "20e7d157eefa4b3615e176f61cad5543584188c9d890209f192c0118f162c77f"
        , "4259dc4c6aec293913bd59211a9a876cad93d7f7961e027de731e48171688ddb"
        , "00006fe28c0ab6f1b372c1a6a246ae63f74f931e8365e15a089c68d619000000"
        , "00000cf85d00042b00010322d0e43b3d92d30ed187f4e101a9a9605c3ee5fc97"
        , "21e6dac3ce3d7732fbb13e037f990e61acee8a7697966afd29dd88f3b1f8a7b1"
        , "4d625c4f8742bd952003a59003359448149bdca64024ab6c2243ab555f9313aa"
        , "49ee1da0b1dd0ce1c575b2cca202dfb90cceb72720dd3d3ea168e53ed64d506e"
        , "4c277fde46946843ee8eab54ab3b"
        ])
      "f753e841682abc578fd12650fe47dac6094ab7882d47026bd3b6055e9060e993"
    -- 850043x3419x1
  , AnnVector (mconcat
        [ "0269559b5385e499d1ec6283c2ec0a0a5b93d45f6cce2d5f69709a9fd3382722"
        , "063a230e7c0fca1b198ed81fc36658c259f70475074f336c7d146d7cb4042de2"
        , "74162681acf44507e98656d6311fc2a8a85ef7bebd34fde195c244623704b8bf"
        , "77d8e2965bdbc7ae483d8d2e2ef41cee147ada06440c1501af0a502f54f8d060"
        , "3ed003cb101583b0986c54994f985e88b730f80a600706b6ac1de2b8030e3961"
        , "388ad7d4a6d7b0d2718374f52ef4b4ab7f7523ba839d922f362613a283615328"
        , "54b9165b22d4f86be8f7503f0149b36db4bbba3496bcb6fb989c558bea04c2d0"
        , "1e6fbddfbec938a1c9fdd841339b7f5ecc40e33cc1df7da7b76da52f72078e7d"
        , "00006fe28c0ab6f1b372c1a6a246ae63f74f931e8365e15a089c68d619000000"
        , "00000cf87b000d5b0001020b831768ded7615eef25b34639c14b912496137ff5"
        , "218ad3277d0efd57a32e0902bb5e47ee26aca1654ce092d9d06eac15d953210c"
        , "737f2220e2d7e54c4f10867703a9be624c84cde9e7452b108942cb7c3c642a1c"
        , "37ab6468b031c605bb103cb5ab038ace825c070bc5af8d326b534ef6ab38ec04"
        , "adbbfa859b00369cc86fbacc3e38"
        ])
      "473c18a52a826efc39367a38a98d9226511289f7e33c38906faeecf890ecdf0f"
    -- 850292x3675x0
  , AnnVector (mconcat
        [ "76d849642499e76439884821fe95628a274e059a77a5cad5039836d40a63cc52"
        , "04aa8748f7a284eaf0e22450d37b51feb293391ae2460ce328826af35ab9da8e"
        , "e052bd9578c74e5a0180294141d66f8f78ec2928456db4eab4062f31c1666bd9"
        , "30a220d211b8a438f9ad9c9169e3bd197ab1452b784f91f22529d4144fc19c28"
        , "c0a83de0ec63b53e71f119b37983f687ee947521b8c9d044917fbb8a0e9a8c2a"
        , "26ac05a821fdf1bac3a764ab9f08c293ed0b01e6dad17ad4da991b403cdfe576"
        , "d49813544db805ae2afbe85572915e4145df0dc55cfd15396bd6f4abbbc9a052"
        , "4e8577ed60f5d058706ca9e3fb970d54b707c598e258727b8da2df2a168f4f42"
        , "00006fe28c0ab6f1b372c1a6a246ae63f74f931e8365e15a089c68d619000000"
        , "00000cf974000e5b0000020cd843f3128386ac5551647496d57aeb0b5259611d"
        , "7ebb9314cae7711565354803bd743abf0f94b6d208d1c3c47aa8e958d6ddd8aa"
        , "4e8c910c6e1d6b8dd7413a93024c95a299224252655af8a9a8d07da1313b8efc"
        , "1dda8964d652ccb81ac5253e0d039404ec864e4efc233af1c781c872048493bc"
        , "df6d284746293cf5f2b2790af0de"
        ])
      "d0f087817bde3d17c5c6f9eb5a9d7c0d14f2498d0c58ebfbd4e8bccfa536ff05"
    -- 853240x888x2
  , AnnVector (mconcat
        [ "f4e2a166440900d15e319812df9c5ffeab61e15f417febb18c1fc07dcc701700"
        , "3fecf1c075d39f4e134e5061a00de59161378f6855cefa71687f710678cc4c27"
        , "e010bc600453f7ac4b84c5f5243dc2b6447af512110c1ef11173a1498a71bc6f"
        , "535607ccf9f64e87ea9641413bd3ad2ec7d96d711f5de0106bf23fd172215f18"
        , "fd12d34bffebe0d64900e944b4fd7ad81f26adb5ed4ea0099ef3c53e09034178"
        , "693af5a55308d341e8af0dddd19eed46438174cdc2e9c65f9ff14844c4d991c4"
        , "37f97805f017d56205f387b24f22d620ea474ba6f837ae114cef015022d55841"
        , "2eca08400137adc856e80254b61783a77941f9550a0d008ef567c0acfa8e9155"
        , "00006fe28c0ab6f1b372c1a6a246ae63f74f931e8365e15a089c68d619000000"
        , "00000d04f8000378000202ad898fce14604311ebe871e1053fa248a25e683803"
        , "a8e599f168fba79bad5f5202c2ecfd7e7646f7a249491c565737b5c9f83863c2"
        , "3b88e752f2d624e026f5fde702519af7e8fa7c6c47858108d1ab7eb9b531a594"
        , "34314c804174ccb0bb658d0f5e03562b849da8a6e3f17d1585f53011eabeb53d"
        , "b6eb23066af87bb81e8adbd0eb1a"
        ])
      "cd04212b8a16a7367adb8314e37698b50ad54a9c901d21586fdd1420fba78431"
    -- 855691x1739x0
  , AnnVector (mconcat
        [ "2bd9078e1b73e459703af26014f26f43426961f70e0f289545559bcc5f75cd74"
        , "5dc4beba17a15301df594267c92fd9e563d75982cc406e095162bdd931107d84"
        , "e98528d5322f18e203805b372049a2eb32aadd3a26ad2fa733a06279635e8dca"
        , "4d778a9c4a1ae260ae581562bd5e57298db70e48308ee2ba8c8bdb91db062e21"
        , "41e5a4396bdc112449c9d2f572f88c1fa610620825ac3fa6bb04a1bfd6cc34a4"
        , "5fb5a00ec08371146305eb845d2ea98b4f60c8bff4e5e589315c34c6af95863e"
        , "6c01bdbd7c589bc448b4f0805e9756b2dcfcd5812df8b507cbf2ae488d6519a3"
        , "12841bcf0f3707f1934b00f845cbe05b67d085d1a4367c8820659285e8760952"
        , "00006fe28c0ab6f1b372c1a6a246ae63f74f931e8365e15a089c68d619000000"
        , "00000d0e8b0006cb000002439112f98b6f4828e1a7426bfbece166bd36837d7d"
        , "ea4225a75ebd1d96864a070324ba2392e25bff76abd0b1f7e4b53b5f82aa53fd"
        , "dc3419b051b6c801db9e2247039adc288b1728f289ca4ac30e4b258a19f776c2"
        , "0e3190f259d759cd672c5f943803e45a0234b0649ba3d599e10df0339a36c140"
        , "652b5efa18b7c17fadfd5b754888"
        ])
      "0092fb593a97482cd7a0dc9ed4954e90eb1ca0700ca55f7a0183674a5faf27ea"
  ]

mainnet_updates :: [UpdateVector]
mainnet_updates = [
    -- 850052x2992x1, 148 bytes, channel_flags 0
    UpdateVector (mconcat
        [ "1c3f0859cb3f4494919c3dcc80d22bbf1e25e378bd95f071530833c2371c33d7"
        , "0766012ebef1cd79b299a54d0d29bea2ef6feca1d6cfa188864c95d7eea7e9a4"
        , "6fe28c0ab6f1b372c1a6a246ae63f74f931e8365e15a089c68d6190000000000"
        , "0cf884000bb000016a0a1c760100006400000000000003e80000000000001388"
        , "00000000b1069a80fdd9030800000000ffffffed"
        ])
      "b1989933c26561ab2cdcc49b471aee5f6e0403a5eb7b88a908cb19cfee0556b9"
      0xf0a89fda 0
    -- 850052x2992x1, 136 bytes, channel_flags 3
  , UpdateVector (mconcat
        [ "261e3d1f1b77b4a606c20dbe5b395e35703adc82fd9320b88e892f51547b93e9"
        , "060f793d8cfd93d2063060a692c31749d0eb3dd351f3e4bbc562d00f89472a42"
        , "6fe28c0ab6f1b372c1a6a246ae63f74f931e8365e15a089c68d6190000000000"
        , "0cf884000bb0000169e321d20103005000000000000003e8000003e800000320"
        , "00000000b1069a80"
        ])
      "5c9960b6fef9fa0d53073895420159e856c7c5f33fdeb694b163a832efe6b8e3"
      0x1da953bd 0
    -- 850013x1067x1, 136 bytes, channel_flags 1
  , UpdateVector (mconcat
        [ "7619128817aadc3c4da18103c468f8c0869f03394e815a7386a81bf74eabb76f"
        , "357c67e0353b25b173bb67a57185db0d39b8bc3736538e58376ccd184cadf539"
        , "6fe28c0ab6f1b372c1a6a246ae63f74f931e8365e15a089c68d6190000000000"
        , "0cf85d00042b000169e334b80101005000000000000003e8000003e800000001"
        , "00000022ce4c6080"
        ])
      "b3d7e24cfbf4d578e3d8ded39d2885fec322c57355e7a513879c72665014e094"
      0xb28659fc 1
    -- 850043x3419x1, 136 bytes, channel_flags 0
  , UpdateVector (mconcat
        [ "f0b2facf82a20cb2c33b12337fd54f4553decff2abc86c20fb7059b890d2f16f"
        , "11d604798b3829ba0367c1898ea21fbb0d50570e70812119ecc069ecde21785a"
        , "6fe28c0ab6f1b372c1a6a246ae63f74f931e8365e15a089c68d6190000000000"
        , "0cf87b000d5b000169e3810f0100006400000000000f424000000000000000c3"
        , "000000001d8119c0"
        ])
      "1b2702991ef2d85cc3dffdc390a3a597f3651d00803e6e08581fd8140694ddf4"
      0x036645d4 2
    -- 850043x3419x1, 148 bytes, channel_flags 1
  , UpdateVector (mconcat
        [ "c93858ef07309fe98c1690521634ad3a55c715500cdf4d31eef8d6f412613c0a"
        , "491df28ec2e7c4a5288a5bf2848d18486d9cf4eeb3f41befeaedf1e121416438"
        , "6fe28c0ab6f1b372c1a6a246ae63f74f931e8365e15a089c68d6190000000000"
        , "0cf87b000d5b000169e3810f0101006400000000000003e80000000000001388"
        , "000000009ae5c730fdd9030800000000fffffa95"
        ])
      "6deeb53e1859601e9b2b859dccace2dd9f85dc6d0605d6d7a966329d2633df1c"
      0x878e5ea4 2
    -- 850052x2992x1, 148 bytes, channel_flags 2
  , UpdateVector (mconcat
        [ "3bf18f613f864d7b9907c98915cf4cbec8d4ac5245f5c66ca3d93571074d8c4f"
        , "3cbc7963377020145f9645ead56f462537384e43cba27ac4b73d9e734275fe6a"
        , "6fe28c0ab6f1b372c1a6a246ae63f74f931e8365e15a089c68d6190000000000"
        , "0cf884000bb0000169e3478f0102006400000000000003e80000000000001066"
        , "00000000b1069a80fdd9030800000000fffffab8"
        ])
      "3af8a43926c24d2707cecf621fb7906cc1feb4c6880b0fde3de19bb5da7e5649"
      0x5ba7913c 0
    -- 850292x3675x0, 136 bytes, channel_flags 2
  , UpdateVector (mconcat
        [ "ee28b4d0ce3abded3c43d5d990a40020b255df072cc76a9fdfd8482aed0c81c1"
        , "02e7e124dcd74acaaf5cf0773cbd10a828b721925cb54f7569cca335f6a9237c"
        , "6fe28c0ab6f1b372c1a6a246ae63f74f931e8365e15a089c68d6190000000000"
        , "0cf974000e5b000069e37c460102002800000000000003e8000003e800000001"
        , "0000000076046700"
        ])
      "43949a9fa1993b3375b7fc2e3a35e35ceb28766907f3d738fa8f46b9ba02474f"
      0x9d4f0032 3
    -- 853240x888x2, 148 bytes, channel_flags 3
  , UpdateVector (mconcat
        [ "c5cfd6f873caf8ad9207f10db0b618bbf4db86e31b64ad6d0d2aca0a14de8899"
        , "261b3b94e7eff8317f77c3fee62941e660361dd3ed4213a94a7bb13fee71dcc8"
        , "6fe28c0ab6f1b372c1a6a246ae63f74f931e8365e15a089c68d6190000000000"
        , "0d04f8000378000269e32992010300900000000000005208000000000000017a"
        , "000000001dcd6500fdd903080000000000000000"
        ])
      "a6d2ea4523ee28ad1f117ac2cffbb6bf29aa5cc6f7f58d2dc058a999563c9560"
      0x172a10e7 4
    -- 855691x1739x0, 136 bytes, channel_flags 0
  , UpdateVector (mconcat
        [ "0f325a044bff29a14636b22023d2a44abf939e34b776020907627b4c72488ed9"
        , "70b357c059e7a110611bb2093d9fffdd00028a84fc08aea155a57d4b79434410"
        , "6fe28c0ab6f1b372c1a6a246ae63f74f931e8365e15a089c68d6190000000000"
        , "0d0e8b0006cb000069e046c70100004800000000000003e80000000000000147"
        , "0000000000000000"
        ])
      "d00785fd5793d86401fb54de019d4094fbff37416d9f611a55fc8fe9a5f536c4"
      0x368d3c11 5
    -- 850013x1067x1, 148 bytes, channel_flags 0
  , UpdateVector (mconcat
        [ "2faeaef5708bde4ce33a2145bca38544d9d3c4431c6e10210f6fab36c37306f2"
        , "53ab90c2d5ec3710c6f481dab4735e398fa83eec94b2d5a9bd41df26a92bce7b"
        , "6fe28c0ab6f1b372c1a6a246ae63f74f931e8365e15a089c68d6190000000000"
        , "0cf85d00042b000169e33b320100009000000000000003e8000000000000109a"
        , "00000022ce4c6080fdd903080000000000000000"
        ])
      "1acb192f08b4dfb70359cebc337e6ed2cc985b74e70dacc9ecdc1bd957a56923"
      0xf4948aaf 1
    -- 850292x3675x0, 136 bytes, channel_flags 1
  , UpdateVector (mconcat
        [ "6bbb96e7af06e5a35815d7bfc1777ee680cb7d2f728855cc8373baf469e1d375"
        , "3aa3f7023a796ead27e8fc37e9509358c6e8c2e9ee6a71db5c8dc9c044a3395c"
        , "6fe28c0ab6f1b372c1a6a246ae63f74f931e8365e15a089c68d6190000000000"
        , "0cf974000e5b000069e37c840101005000000000000003e80000000000000064"
        , "0000000076046700"
        ])
      "cf3744188962bb9ab7a9847f9b21624c488ec250e780c728c1f9d0032d2ff3a2"
      0xe7e9397e 3
    -- 853240x888x2, 136 bytes, channel_flags 0
  , UpdateVector (mconcat
        [ "49ac69d15fbdb35e1022bbe10d4c319fcc981c488e8120a6c6630caa81d96326"
        , "335ac4948bbaa6bd24811550700c98fcb6af26f00f173d94fa1f060962e8d829"
        , "6fe28c0ab6f1b372c1a6a246ae63f74f931e8365e15a089c68d6190000000000"
        , "0d04f8000378000269dea9690100005000000000000003e8000003e8000003e7"
        , "00000000ec08ce00"
        ])
      "14e2befbe15ea11b5094628ce2ceb1e22a325ed7a7fc03300641e0d7785e32f1"
      0xe265c205 4
    -- 855691x1739x0, 136 bytes, channel_flags 1
  , UpdateVector (mconcat
        [ "337910ec557c3c04ad42130175e9c783179bb6e0627b36e96b5c635795e4f500"
        , "2f11d94319a52c930fd166643709f1b97c096c64cbf412f620f53158c45054eb"
        , "6fe28c0ab6f1b372c1a6a246ae63f74f931e8365e15a089c68d6190000000000"
        , "0d0e8b0006cb000069e2cb460101005000000000000003e8000000000000000a"
        , "000000012a05f200"
        ])
      "bc08450188a2eb2f1e4fa964a5f11d458a06ce4a00010f86980fbb68c7d8f033"
      0x822c67ea 5
  ]

-- node_announcement ----------------------------------------------------------

-- | A node_announcement payload signed with the secret key 0x4242..42,
--   and its signature digest (computed independently of this library).
--   It carries one descriptor of each known type, a descriptor of the
--   unknown type 6, and an extension record of type 101.
node_announcement_vector :: (BS.ByteString, BS.ByteString)
node_announcement_vector = (mconcat
    [ "edbd2df19aee2f072d9665e2d9a2807fbcd4be6876f6863955ea11870e411e66"
    , "063a31dcef14f1983524f87e108ee556b3a6646c873cd4bbcc07512c8f5e8f6a"
    , "000908000000000002aaa26553f1000324653eac434488002cc06bbfb7f10fe1"
    , "8991e35f9fe4302dbea6d2353dc0ab1c314159707061642d626f6c7437000000"
    , "000000000000000000000000000000000000000063017f000001260702000000"
    , "00000000000000000000000001260703aaaaaaaaaaaaaaaaaaaaaaaa04bbbbbb"
    , "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
    , "2607050e6c6e2e6578616d706c652e636f6d2607060102036503657874"
    ]
  , "c81748f1075dce2da919ce8a52fe59fec409aa10208b8ffc227f5292af5f0abf")

-- extended gossip queries ----------------------------------------------------

-- | The vectors of bolt07/extended-queries.json, in order: messages with
--   their 2-byte type, in hex.
query_vectors :: [BS.ByteString]
query_vectors = [
    -- 0: QueryChannelRange
    mconcat
      [ "01070f9188f13cb7b2c71f2a335e3a4fc328bf5beb436012afca590b1a11466e"
      , "2206000186a0000005dc"
      ]
    -- 1: QueryChannelRange
  , mconcat
      [ "01070f9188f13cb7b2c71f2a335e3a4fc328bf5beb436012afca590b1a11466e"
      , "2206000088b800000064010103"
      ]
    -- 2: ReplyChannelRange, UNCOMPRESSED
  , mconcat
      [ "01080f9188f13cb7b2c71f2a335e3a4fc328bf5beb436012afca590b1a11466e"
      , "2206000b8a06000005dc01001900000000000000008e0000000000003c690000"
      , "00000045a6c4"
      ]
    -- 3: ReplyChannelRange, COMPRESSED_ZLIB
  , mconcat
      [ "01080f9188f13cb7b2c71f2a335e3a4fc328bf5beb436012afca590b1a11466e"
      , "2206000006400000006e01001601789c636000833e08659309a65878be010010"
      , "a9023a"
      ]
    -- 4: ReplyChannelRange, UNCOMPRESSED
  , mconcat
      [ "01080f9188f13cb7b2c71f2a335e3a4fc328bf5beb436012afca590b1a11466e"
      , "22060001ddde000005dc01001900000000000000304300000000000778d60000"
      , "00000046e1c1011900000282c1000e77c5000778ad00490ab00000b57800955b"
      , "ff031800000457000008ae00000d050000115c000015b300001a0a"
      ]
    -- 5: ReplyChannelRange, COMPRESSED_ZLIB
  , mconcat
      [ "01080f9188f13cb7b2c71f2a335e3a4fc328bf5beb436012afca590b1a11466e"
      , "22060001ddde000005dc01001801789c63600001036730c55e710d4cbb3d3c08"
      , "0017c303b1012201789c63606a3ac8c0577e9481bd622d8327d7060686ad150c"
      , "53a3ff0300554707db031800000457000008ae00000d050000115c000015b300"
      , "001a0a"
      ]
    -- 6: QueryShortChannelIds, UNCOMPRESSED
  , mconcat
      [ "01050f9188f13cb7b2c71f2a335e3a4fc328bf5beb436012afca590b1a11466e"
      , "2206001900000000000000008e0000000000003c69000000000045a6c4"
      ]
    -- 7: QueryShortChannelIds, COMPRESSED_ZLIB
  , mconcat
      [ "01050f9188f13cb7b2c71f2a335e3a4fc328bf5beb436012afca590b1a11466e"
      , "2206001801789c63600001c12b608a69e73e30edbaec0800203b040e"
      ]
    -- 8: QueryShortChannelIds, UNCOMPRESSED
  , mconcat
      [ "01050f9188f13cb7b2c71f2a335e3a4fc328bf5beb436012afca590b1a11466e"
      , "22060019000000000000002fc80000000000003cc4000000000045a6c4010c01"
      , "789c6364620100000e0008"
      ]
    -- 9: QueryShortChannelIds, COMPRESSED_ZLIB
  , mconcat
      [ "01050f9188f13cb7b2c71f2a335e3a4fc328bf5beb436012afca590b1a11466e"
      , "2206001801789c63600001f30a30c5b0cd144cb92e3b020017c6034a010c0178"
      , "9c6364620100000e0008"
      ]
  ]
