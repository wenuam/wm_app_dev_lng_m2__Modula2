IMPLEMENTATION MODULE AES;
<*/OPTIMIZE:T*>
(*
  this module source adapted to Modula-2 from the Crypto++ 4.2 library.

// rijndael.cpp - modified by Chris Morgan <cmorgan@wpi.edu>
// and Wei Dai from Paulo Baretto's Rijndael implementation
// The original code and all modifications are in the public domain.

// This is the original introductory comment:

/**
 * version 3.0 (December 2000)
 *
 * Optimised ANSI C code for the Rijndael cipher (now AES)
 *
 * author Vincent Rijmen <vincent.rijmen@esat.kuleuven.ac.be>
 * author Antoon Bosselaers <antoon.bosselaers@esat.kuleuven.ac.be>
 * author Paulo Barreto <paulo.barreto@terra.com.br>
 *
 * This code is hereby placed in the public domain.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHORS ''AS IS'' AND ANY EXPRESS
 * OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
 * BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
 * OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
 * EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
*)

FROM SYSTEM IMPORT
    BYTE, ADDRESS, ADRCARD, ADR, ADDADR, BIGENDIAN;

FROM ExStorage IMPORT
    ALLOCATE, DeallocateEx, HeapInfoPointer, GetHeap;

<*/VALIDVER:BIGTABLE*>
(*<*/VER:BIGTABLE*>*)(*these big constants might be RISC processor unfriendly*)

TYPE
    AESRec =
        RECORD
        (* keys large enough for 256-bit, 14-round operation *)
        rke, rkd        : ARRAY [0..59] OF CARDINAL32;(* round keys*)
        rounds          : CARDINAL;
        iv              : IV;
        heap            : HeapInfoPointer;
        END;
    AES         = POINTER TO AESRec;

    DataBlock   = ARRAY [0..(BlockSize/4)-1] OF CARDINAL32;

CONST
    rcon : ARRAY [0..9] OF CARDINAL32 =
        {
        001000000h, 002000000h, 004000000h, 008000000h,
        010000000h, 020000000h, 040000000h, 080000000h,
        01B000000h, 036000000h
        (* for 128-bit blocks, Rijndael never uses more than 10 rcon values *)
        };

    Te0 : ARRAY [0..255] OF CARDINAL32 =
    {
    0c66363a5h, 0f87c7c84h, 0ee777799h, 0f67b7b8dh,
    0fff2f20dh, 0d66b6bbdh, 0de6f6fb1h, 091c5c554h,
    060303050h, 002010103h, 0ce6767a9h, 0562b2b7dh,
    0e7fefe19h, 0b5d7d762h, 04dababe6h, 0ec76769ah,
    08fcaca45h, 01f82829dh, 089c9c940h, 0fa7d7d87h,
    0effafa15h, 0b25959ebh, 08e4747c9h, 0fbf0f00bh,
    041adadech, 0b3d4d467h, 05fa2a2fdh, 045afafeah,
    0239c9cbfh, 053a4a4f7h, 0e4727296h, 09bc0c05bh,
    075b7b7c2h, 0e1fdfd1ch, 03d9393aeh, 04c26266ah,
    06c36365ah, 07e3f3f41h, 0f5f7f702h, 083cccc4fh,
    06834345ch, 051a5a5f4h, 0d1e5e534h, 0f9f1f108h,
    0e2717193h, 0abd8d873h, 062313153h, 02a15153fh,
    00804040ch, 095c7c752h, 046232365h, 09dc3c35eh,
    030181828h, 0379696a1h, 00a05050fh, 02f9a9ab5h,
    00e070709h, 024121236h, 01b80809bh, 0dfe2e23dh,
    0cdebeb26h, 04e272769h, 07fb2b2cdh, 0ea75759fh,
    01209091bh, 01d83839eh, 0582c2c74h, 0341a1a2eh,
    0361b1b2dh, 0dc6e6eb2h, 0b45a5aeeh, 05ba0a0fbh,
    0a45252f6h, 0763b3b4dh, 0b7d6d661h, 07db3b3ceh,
    05229297bh, 0dde3e33eh, 05e2f2f71h, 013848497h,
    0a65353f5h, 0b9d1d168h, 000000000h, 0c1eded2ch,
    040202060h, 0e3fcfc1fh, 079b1b1c8h, 0b65b5bedh,
    0d46a6abeh, 08dcbcb46h, 067bebed9h, 07239394bh,
    0944a4adeh, 0984c4cd4h, 0b05858e8h, 085cfcf4ah,
    0bbd0d06bh, 0c5efef2ah, 04faaaae5h, 0edfbfb16h,
    0864343c5h, 09a4d4dd7h, 066333355h, 011858594h,
    08a4545cfh, 0e9f9f910h, 004020206h, 0fe7f7f81h,
    0a05050f0h, 0783c3c44h, 0259f9fbah, 04ba8a8e3h,
    0a25151f3h, 05da3a3feh, 0804040c0h, 0058f8f8ah,
    03f9292adh, 0219d9dbch, 070383848h, 0f1f5f504h,
    063bcbcdfh, 077b6b6c1h, 0afdada75h, 042212163h,
    020101030h, 0e5ffff1ah, 0fdf3f30eh, 0bfd2d26dh,
    081cdcd4ch, 0180c0c14h, 026131335h, 0c3ecec2fh,
    0be5f5fe1h, 0359797a2h, 0884444cch, 02e171739h,
    093c4c457h, 055a7a7f2h, 0fc7e7e82h, 07a3d3d47h,
    0c86464ach, 0ba5d5de7h, 03219192bh, 0e6737395h,
    0c06060a0h, 019818198h, 09e4f4fd1h, 0a3dcdc7fh,
    044222266h, 0542a2a7eh, 03b9090abh, 00b888883h,
    08c4646cah, 0c7eeee29h, 06bb8b8d3h, 02814143ch,
    0a7dede79h, 0bc5e5ee2h, 0160b0b1dh, 0addbdb76h,
    0dbe0e03bh, 064323256h, 0743a3a4eh, 0140a0a1eh,
    0924949dbh, 00c06060ah, 04824246ch, 0b85c5ce4h,
    09fc2c25dh, 0bdd3d36eh, 043acacefh, 0c46262a6h,
    0399191a8h, 0319595a4h, 0d3e4e437h, 0f279798bh,
    0d5e7e732h, 08bc8c843h, 06e373759h, 0da6d6db7h,
    0018d8d8ch, 0b1d5d564h, 09c4e4ed2h, 049a9a9e0h,
    0d86c6cb4h, 0ac5656fah, 0f3f4f407h, 0cfeaea25h,
    0ca6565afh, 0f47a7a8eh, 047aeaee9h, 010080818h,
    06fbabad5h, 0f0787888h, 04a25256fh, 05c2e2e72h,
    0381c1c24h, 057a6a6f1h, 073b4b4c7h, 097c6c651h,
    0cbe8e823h, 0a1dddd7ch, 0e874749ch, 03e1f1f21h,
    0964b4bddh, 061bdbddch, 00d8b8b86h, 00f8a8a85h,
    0e0707090h, 07c3e3e42h, 071b5b5c4h, 0cc6666aah,
    0904848d8h, 006030305h, 0f7f6f601h, 01c0e0e12h,
    0c26161a3h, 06a35355fh, 0ae5757f9h, 069b9b9d0h,
    017868691h, 099c1c158h, 03a1d1d27h, 0279e9eb9h,
    0d9e1e138h, 0ebf8f813h, 02b9898b3h, 022111133h,
    0d26969bbh, 0a9d9d970h, 0078e8e89h, 0339494a7h,
    02d9b9bb6h, 03c1e1e22h, 015878792h, 0c9e9e920h,
    087cece49h, 0aa5555ffh, 050282878h, 0a5dfdf7ah,
    0038c8c8fh, 059a1a1f8h, 009898980h, 01a0d0d17h,
    065bfbfdah, 0d7e6e631h, 0844242c6h, 0d06868b8h,
    0824141c3h, 0299999b0h, 05a2d2d77h, 01e0f0f11h,
    07bb0b0cbh, 0a85454fch, 06dbbbbd6h, 02c16163ah
    };

    Te1 : ARRAY [0..255] OF CARDINAL32 =
    {
    0a5c66363h, 084f87c7ch, 099ee7777h, 08df67b7bh,
    00dfff2f2h, 0bdd66b6bh, 0b1de6f6fh, 05491c5c5h,
    050603030h, 003020101h, 0a9ce6767h, 07d562b2bh,
    019e7fefeh, 062b5d7d7h, 0e64dababh, 09aec7676h,
    0458fcacah, 09d1f8282h, 04089c9c9h, 087fa7d7dh,
    015effafah, 0ebb25959h, 0c98e4747h, 00bfbf0f0h,
    0ec41adadh, 067b3d4d4h, 0fd5fa2a2h, 0ea45afafh,
    0bf239c9ch, 0f753a4a4h, 096e47272h, 05b9bc0c0h,
    0c275b7b7h, 01ce1fdfdh, 0ae3d9393h, 06a4c2626h,
    05a6c3636h, 0417e3f3fh, 002f5f7f7h, 04f83cccch,
    05c683434h, 0f451a5a5h, 034d1e5e5h, 008f9f1f1h,
    093e27171h, 073abd8d8h, 053623131h, 03f2a1515h,
    00c080404h, 05295c7c7h, 065462323h, 05e9dc3c3h,
    028301818h, 0a1379696h, 00f0a0505h, 0b52f9a9ah,
    0090e0707h, 036241212h, 09b1b8080h, 03ddfe2e2h,
    026cdebebh, 0694e2727h, 0cd7fb2b2h, 09fea7575h,
    01b120909h, 09e1d8383h, 074582c2ch, 02e341a1ah,
    02d361b1bh, 0b2dc6e6eh, 0eeb45a5ah, 0fb5ba0a0h,
    0f6a45252h, 04d763b3bh, 061b7d6d6h, 0ce7db3b3h,
    07b522929h, 03edde3e3h, 0715e2f2fh, 097138484h,
    0f5a65353h, 068b9d1d1h, 000000000h, 02cc1ededh,
    060402020h, 01fe3fcfch, 0c879b1b1h, 0edb65b5bh,
    0bed46a6ah, 0468dcbcbh, 0d967bebeh, 04b723939h,
    0de944a4ah, 0d4984c4ch, 0e8b05858h, 04a85cfcfh,
    06bbbd0d0h, 02ac5efefh, 0e54faaaah, 016edfbfbh,
    0c5864343h, 0d79a4d4dh, 055663333h, 094118585h,
    0cf8a4545h, 010e9f9f9h, 006040202h, 081fe7f7fh,
    0f0a05050h, 044783c3ch, 0ba259f9fh, 0e34ba8a8h,
    0f3a25151h, 0fe5da3a3h, 0c0804040h, 08a058f8fh,
    0ad3f9292h, 0bc219d9dh, 048703838h, 004f1f5f5h,
    0df63bcbch, 0c177b6b6h, 075afdadah, 063422121h,
    030201010h, 01ae5ffffh, 00efdf3f3h, 06dbfd2d2h,
    04c81cdcdh, 014180c0ch, 035261313h, 02fc3ecech,
    0e1be5f5fh, 0a2359797h, 0cc884444h, 0392e1717h,
    05793c4c4h, 0f255a7a7h, 082fc7e7eh, 0477a3d3dh,
    0acc86464h, 0e7ba5d5dh, 02b321919h, 095e67373h,
    0a0c06060h, 098198181h, 0d19e4f4fh, 07fa3dcdch,
    066442222h, 07e542a2ah, 0ab3b9090h, 0830b8888h,
    0ca8c4646h, 029c7eeeeh, 0d36bb8b8h, 03c281414h,
    079a7dedeh, 0e2bc5e5eh, 01d160b0bh, 076addbdbh,
    03bdbe0e0h, 056643232h, 04e743a3ah, 01e140a0ah,
    0db924949h, 00a0c0606h, 06c482424h, 0e4b85c5ch,
    05d9fc2c2h, 06ebdd3d3h, 0ef43acach, 0a6c46262h,
    0a8399191h, 0a4319595h, 037d3e4e4h, 08bf27979h,
    032d5e7e7h, 0438bc8c8h, 0596e3737h, 0b7da6d6dh,
    08c018d8dh, 064b1d5d5h, 0d29c4e4eh, 0e049a9a9h,
    0b4d86c6ch, 0faac5656h, 007f3f4f4h, 025cfeaeah,
    0afca6565h, 08ef47a7ah, 0e947aeaeh, 018100808h,
    0d56fbabah, 088f07878h, 06f4a2525h, 0725c2e2eh,
    024381c1ch, 0f157a6a6h, 0c773b4b4h, 05197c6c6h,
    023cbe8e8h, 07ca1ddddh, 09ce87474h, 0213e1f1fh,
    0dd964b4bh, 0dc61bdbdh, 0860d8b8bh, 0850f8a8ah,
    090e07070h, 0427c3e3eh, 0c471b5b5h, 0aacc6666h,
    0d8904848h, 005060303h, 001f7f6f6h, 0121c0e0eh,
    0a3c26161h, 05f6a3535h, 0f9ae5757h, 0d069b9b9h,
    091178686h, 05899c1c1h, 0273a1d1dh, 0b9279e9eh,
    038d9e1e1h, 013ebf8f8h, 0b32b9898h, 033221111h,
    0bbd26969h, 070a9d9d9h, 089078e8eh, 0a7339494h,
    0b62d9b9bh, 0223c1e1eh, 092158787h, 020c9e9e9h,
    04987ceceh, 0ffaa5555h, 078502828h, 07aa5dfdfh,
    08f038c8ch, 0f859a1a1h, 080098989h, 0171a0d0dh,
    0da65bfbfh, 031d7e6e6h, 0c6844242h, 0b8d06868h,
    0c3824141h, 0b0299999h, 0775a2d2dh, 0111e0f0fh,
    0cb7bb0b0h, 0fca85454h, 0d66dbbbbh, 03a2c1616h
    };

    Te2 : ARRAY [0..255] OF CARDINAL32 =
    {
    063a5c663h, 07c84f87ch, 07799ee77h, 07b8df67bh,
    0f20dfff2h, 06bbdd66bh, 06fb1de6fh, 0c55491c5h,
    030506030h, 001030201h, 067a9ce67h, 02b7d562bh,
    0fe19e7feh, 0d762b5d7h, 0abe64dabh, 0769aec76h,
    0ca458fcah, 0829d1f82h, 0c94089c9h, 07d87fa7dh,
    0fa15effah, 059ebb259h, 047c98e47h, 0f00bfbf0h,
    0adec41adh, 0d467b3d4h, 0a2fd5fa2h, 0afea45afh,
    09cbf239ch, 0a4f753a4h, 07296e472h, 0c05b9bc0h,
    0b7c275b7h, 0fd1ce1fdh, 093ae3d93h, 0266a4c26h,
    0365a6c36h, 03f417e3fh, 0f702f5f7h, 0cc4f83cch,
    0345c6834h, 0a5f451a5h, 0e534d1e5h, 0f108f9f1h,
    07193e271h, 0d873abd8h, 031536231h, 0153f2a15h,
    0040c0804h, 0c75295c7h, 023654623h, 0c35e9dc3h,
    018283018h, 096a13796h, 0050f0a05h, 09ab52f9ah,
    007090e07h, 012362412h, 0809b1b80h, 0e23ddfe2h,
    0eb26cdebh, 027694e27h, 0b2cd7fb2h, 0759fea75h,
    0091b1209h, 0839e1d83h, 02c74582ch, 01a2e341ah,
    01b2d361bh, 06eb2dc6eh, 05aeeb45ah, 0a0fb5ba0h,
    052f6a452h, 03b4d763bh, 0d661b7d6h, 0b3ce7db3h,
    0297b5229h, 0e33edde3h, 02f715e2fh, 084971384h,
    053f5a653h, 0d168b9d1h, 000000000h, 0ed2cc1edh,
    020604020h, 0fc1fe3fch, 0b1c879b1h, 05bedb65bh,
    06abed46ah, 0cb468dcbh, 0bed967beh, 0394b7239h,
    04ade944ah, 04cd4984ch, 058e8b058h, 0cf4a85cfh,
    0d06bbbd0h, 0ef2ac5efh, 0aae54faah, 0fb16edfbh,
    043c58643h, 04dd79a4dh, 033556633h, 085941185h,
    045cf8a45h, 0f910e9f9h, 002060402h, 07f81fe7fh,
    050f0a050h, 03c44783ch, 09fba259fh, 0a8e34ba8h,
    051f3a251h, 0a3fe5da3h, 040c08040h, 08f8a058fh,
    092ad3f92h, 09dbc219dh, 038487038h, 0f504f1f5h,
    0bcdf63bch, 0b6c177b6h, 0da75afdah, 021634221h,
    010302010h, 0ff1ae5ffh, 0f30efdf3h, 0d26dbfd2h,
    0cd4c81cdh, 00c14180ch, 013352613h, 0ec2fc3ech,
    05fe1be5fh, 097a23597h, 044cc8844h, 017392e17h,
    0c45793c4h, 0a7f255a7h, 07e82fc7eh, 03d477a3dh,
    064acc864h, 05de7ba5dh, 0192b3219h, 07395e673h,
    060a0c060h, 081981981h, 04fd19e4fh, 0dc7fa3dch,
    022664422h, 02a7e542ah, 090ab3b90h, 088830b88h,
    046ca8c46h, 0ee29c7eeh, 0b8d36bb8h, 0143c2814h,
    0de79a7deh, 05ee2bc5eh, 00b1d160bh, 0db76addbh,
    0e03bdbe0h, 032566432h, 03a4e743ah, 00a1e140ah,
    049db9249h, 0060a0c06h, 0246c4824h, 05ce4b85ch,
    0c25d9fc2h, 0d36ebdd3h, 0acef43ach, 062a6c462h,
    091a83991h, 095a43195h, 0e437d3e4h, 0798bf279h,
    0e732d5e7h, 0c8438bc8h, 037596e37h, 06db7da6dh,
    08d8c018dh, 0d564b1d5h, 04ed29c4eh, 0a9e049a9h,
    06cb4d86ch, 056faac56h, 0f407f3f4h, 0ea25cfeah,
    065afca65h, 07a8ef47ah, 0aee947aeh, 008181008h,
    0bad56fbah, 07888f078h, 0256f4a25h, 02e725c2eh,
    01c24381ch, 0a6f157a6h, 0b4c773b4h, 0c65197c6h,
    0e823cbe8h, 0dd7ca1ddh, 0749ce874h, 01f213e1fh,
    04bdd964bh, 0bddc61bdh, 08b860d8bh, 08a850f8ah,
    07090e070h, 03e427c3eh, 0b5c471b5h, 066aacc66h,
    048d89048h, 003050603h, 0f601f7f6h, 00e121c0eh,
    061a3c261h, 0355f6a35h, 057f9ae57h, 0b9d069b9h,
    086911786h, 0c15899c1h, 01d273a1dh, 09eb9279eh,
    0e138d9e1h, 0f813ebf8h, 098b32b98h, 011332211h,
    069bbd269h, 0d970a9d9h, 08e89078eh, 094a73394h,
    09bb62d9bh, 01e223c1eh, 087921587h, 0e920c9e9h,
    0ce4987ceh, 055ffaa55h, 028785028h, 0df7aa5dfh,
    08c8f038ch, 0a1f859a1h, 089800989h, 00d171a0dh,
    0bfda65bfh, 0e631d7e6h, 042c68442h, 068b8d068h,
    041c38241h, 099b02999h, 02d775a2dh, 00f111e0fh,
    0b0cb7bb0h, 054fca854h, 0bbd66dbbh, 0163a2c16h
    };

    Te3 : ARRAY [0..255] OF CARDINAL32 =
    {
    06363a5c6h, 07c7c84f8h, 0777799eeh, 07b7b8df6h,
    0f2f20dffh, 06b6bbdd6h, 06f6fb1deh, 0c5c55491h,
    030305060h, 001010302h, 06767a9ceh, 02b2b7d56h,
    0fefe19e7h, 0d7d762b5h, 0ababe64dh, 076769aech,
    0caca458fh, 082829d1fh, 0c9c94089h, 07d7d87fah,
    0fafa15efh, 05959ebb2h, 04747c98eh, 0f0f00bfbh,
    0adadec41h, 0d4d467b3h, 0a2a2fd5fh, 0afafea45h,
    09c9cbf23h, 0a4a4f753h, 0727296e4h, 0c0c05b9bh,
    0b7b7c275h, 0fdfd1ce1h, 09393ae3dh, 026266a4ch,
    036365a6ch, 03f3f417eh, 0f7f702f5h, 0cccc4f83h,
    034345c68h, 0a5a5f451h, 0e5e534d1h, 0f1f108f9h,
    0717193e2h, 0d8d873abh, 031315362h, 015153f2ah,
    004040c08h, 0c7c75295h, 023236546h, 0c3c35e9dh,
    018182830h, 09696a137h, 005050f0ah, 09a9ab52fh,
    00707090eh, 012123624h, 080809b1bh, 0e2e23ddfh,
    0ebeb26cdh, 02727694eh, 0b2b2cd7fh, 075759feah,
    009091b12h, 083839e1dh, 02c2c7458h, 01a1a2e34h,
    01b1b2d36h, 06e6eb2dch, 05a5aeeb4h, 0a0a0fb5bh,
    05252f6a4h, 03b3b4d76h, 0d6d661b7h, 0b3b3ce7dh,
    029297b52h, 0e3e33eddh, 02f2f715eh, 084849713h,
    05353f5a6h, 0d1d168b9h, 000000000h, 0eded2cc1h,
    020206040h, 0fcfc1fe3h, 0b1b1c879h, 05b5bedb6h,
    06a6abed4h, 0cbcb468dh, 0bebed967h, 039394b72h,
    04a4ade94h, 04c4cd498h, 05858e8b0h, 0cfcf4a85h,
    0d0d06bbbh, 0efef2ac5h, 0aaaae54fh, 0fbfb16edh,
    04343c586h, 04d4dd79ah, 033335566h, 085859411h,
    04545cf8ah, 0f9f910e9h, 002020604h, 07f7f81feh,
    05050f0a0h, 03c3c4478h, 09f9fba25h, 0a8a8e34bh,
    05151f3a2h, 0a3a3fe5dh, 04040c080h, 08f8f8a05h,
    09292ad3fh, 09d9dbc21h, 038384870h, 0f5f504f1h,
    0bcbcdf63h, 0b6b6c177h, 0dada75afh, 021216342h,
    010103020h, 0ffff1ae5h, 0f3f30efdh, 0d2d26dbfh,
    0cdcd4c81h, 00c0c1418h, 013133526h, 0ecec2fc3h,
    05f5fe1beh, 09797a235h, 04444cc88h, 01717392eh,
    0c4c45793h, 0a7a7f255h, 07e7e82fch, 03d3d477ah,
    06464acc8h, 05d5de7bah, 019192b32h, 0737395e6h,
    06060a0c0h, 081819819h, 04f4fd19eh, 0dcdc7fa3h,
    022226644h, 02a2a7e54h, 09090ab3bh, 08888830bh,
    04646ca8ch, 0eeee29c7h, 0b8b8d36bh, 014143c28h,
    0dede79a7h, 05e5ee2bch, 00b0b1d16h, 0dbdb76adh,
    0e0e03bdbh, 032325664h, 03a3a4e74h, 00a0a1e14h,
    04949db92h, 006060a0ch, 024246c48h, 05c5ce4b8h,
    0c2c25d9fh, 0d3d36ebdh, 0acacef43h, 06262a6c4h,
    09191a839h, 09595a431h, 0e4e437d3h, 079798bf2h,
    0e7e732d5h, 0c8c8438bh, 03737596eh, 06d6db7dah,
    08d8d8c01h, 0d5d564b1h, 04e4ed29ch, 0a9a9e049h,
    06c6cb4d8h, 05656faach, 0f4f407f3h, 0eaea25cfh,
    06565afcah, 07a7a8ef4h, 0aeaee947h, 008081810h,
    0babad56fh, 0787888f0h, 025256f4ah, 02e2e725ch,
    01c1c2438h, 0a6a6f157h, 0b4b4c773h, 0c6c65197h,
    0e8e823cbh, 0dddd7ca1h, 074749ce8h, 01f1f213eh,
    04b4bdd96h, 0bdbddc61h, 08b8b860dh, 08a8a850fh,
    0707090e0h, 03e3e427ch, 0b5b5c471h, 06666aacch,
    04848d890h, 003030506h, 0f6f601f7h, 00e0e121ch,
    06161a3c2h, 035355f6ah, 05757f9aeh, 0b9b9d069h,
    086869117h, 0c1c15899h, 01d1d273ah, 09e9eb927h,
    0e1e138d9h, 0f8f813ebh, 09898b32bh, 011113322h,
    06969bbd2h, 0d9d970a9h, 08e8e8907h, 09494a733h,
    09b9bb62dh, 01e1e223ch, 087879215h, 0e9e920c9h,
    0cece4987h, 05555ffaah, 028287850h, 0dfdf7aa5h,
    08c8c8f03h, 0a1a1f859h, 089898009h, 00d0d171ah,
    0bfbfda65h, 0e6e631d7h, 04242c684h, 06868b8d0h,
    04141c382h, 09999b029h, 02d2d775ah, 00f0f111eh,
    0b0b0cb7bh, 05454fca8h, 0bbbbd66dh, 016163a2ch
    };

%IF BIGTABLE %THEN
    Te4 : ARRAY [0..255] OF CARDINAL32 =
    {
    063636363h, 07c7c7c7ch, 077777777h, 07b7b7b7bh,
    0f2f2f2f2h, 06b6b6b6bh, 06f6f6f6fh, 0c5c5c5c5h,
    030303030h, 001010101h, 067676767h, 02b2b2b2bh,
    0fefefefeh, 0d7d7d7d7h, 0ababababh, 076767676h,
    0cacacacah, 082828282h, 0c9c9c9c9h, 07d7d7d7dh,
    0fafafafah, 059595959h, 047474747h, 0f0f0f0f0h,
    0adadadadh, 0d4d4d4d4h, 0a2a2a2a2h, 0afafafafh,
    09c9c9c9ch, 0a4a4a4a4h, 072727272h, 0c0c0c0c0h,
    0b7b7b7b7h, 0fdfdfdfdh, 093939393h, 026262626h,
    036363636h, 03f3f3f3fh, 0f7f7f7f7h, 0cccccccch,
    034343434h, 0a5a5a5a5h, 0e5e5e5e5h, 0f1f1f1f1h,
    071717171h, 0d8d8d8d8h, 031313131h, 015151515h,
    004040404h, 0c7c7c7c7h, 023232323h, 0c3c3c3c3h,
    018181818h, 096969696h, 005050505h, 09a9a9a9ah,
    007070707h, 012121212h, 080808080h, 0e2e2e2e2h,
    0ebebebebh, 027272727h, 0b2b2b2b2h, 075757575h,
    009090909h, 083838383h, 02c2c2c2ch, 01a1a1a1ah,
    01b1b1b1bh, 06e6e6e6eh, 05a5a5a5ah, 0a0a0a0a0h,
    052525252h, 03b3b3b3bh, 0d6d6d6d6h, 0b3b3b3b3h,
    029292929h, 0e3e3e3e3h, 02f2f2f2fh, 084848484h,
    053535353h, 0d1d1d1d1h, 000000000h, 0ededededh,
    020202020h, 0fcfcfcfch, 0b1b1b1b1h, 05b5b5b5bh,
    06a6a6a6ah, 0cbcbcbcbh, 0bebebebeh, 039393939h,
    04a4a4a4ah, 04c4c4c4ch, 058585858h, 0cfcfcfcfh,
    0d0d0d0d0h, 0efefefefh, 0aaaaaaaah, 0fbfbfbfbh,
    043434343h, 04d4d4d4dh, 033333333h, 085858585h,
    045454545h, 0f9f9f9f9h, 002020202h, 07f7f7f7fh,
    050505050h, 03c3c3c3ch, 09f9f9f9fh, 0a8a8a8a8h,
    051515151h, 0a3a3a3a3h, 040404040h, 08f8f8f8fh,
    092929292h, 09d9d9d9dh, 038383838h, 0f5f5f5f5h,
    0bcbcbcbch, 0b6b6b6b6h, 0dadadadah, 021212121h,
    010101010h, 0ffffffffh, 0f3f3f3f3h, 0d2d2d2d2h,
    0cdcdcdcdh, 00c0c0c0ch, 013131313h, 0ecececech,
    05f5f5f5fh, 097979797h, 044444444h, 017171717h,
    0c4c4c4c4h, 0a7a7a7a7h, 07e7e7e7eh, 03d3d3d3dh,
    064646464h, 05d5d5d5dh, 019191919h, 073737373h,
    060606060h, 081818181h, 04f4f4f4fh, 0dcdcdcdch,
    022222222h, 02a2a2a2ah, 090909090h, 088888888h,
    046464646h, 0eeeeeeeeh, 0b8b8b8b8h, 014141414h,
    0dedededeh, 05e5e5e5eh, 00b0b0b0bh, 0dbdbdbdbh,
    0e0e0e0e0h, 032323232h, 03a3a3a3ah, 00a0a0a0ah,
    049494949h, 006060606h, 024242424h, 05c5c5c5ch,
    0c2c2c2c2h, 0d3d3d3d3h, 0acacacach, 062626262h,
    091919191h, 095959595h, 0e4e4e4e4h, 079797979h,
    0e7e7e7e7h, 0c8c8c8c8h, 037373737h, 06d6d6d6dh,
    08d8d8d8dh, 0d5d5d5d5h, 04e4e4e4eh, 0a9a9a9a9h,
    06c6c6c6ch, 056565656h, 0f4f4f4f4h, 0eaeaeaeah,
    065656565h, 07a7a7a7ah, 0aeaeaeaeh, 008080808h,
    0babababah, 078787878h, 025252525h, 02e2e2e2eh,
    01c1c1c1ch, 0a6a6a6a6h, 0b4b4b4b4h, 0c6c6c6c6h,
    0e8e8e8e8h, 0ddddddddh, 074747474h, 01f1f1f1fh,
    04b4b4b4bh, 0bdbdbdbdh, 08b8b8b8bh, 08a8a8a8ah,
    070707070h, 03e3e3e3eh, 0b5b5b5b5h, 066666666h,
    048484848h, 003030303h, 0f6f6f6f6h, 00e0e0e0eh,
    061616161h, 035353535h, 057575757h, 0b9b9b9b9h,
    086868686h, 0c1c1c1c1h, 01d1d1d1dh, 09e9e9e9eh,
    0e1e1e1e1h, 0f8f8f8f8h, 098989898h, 011111111h,
    069696969h, 0d9d9d9d9h, 08e8e8e8eh, 094949494h,
    09b9b9b9bh, 01e1e1e1eh, 087878787h, 0e9e9e9e9h,
    0cecececeh, 055555555h, 028282828h, 0dfdfdfdfh,
    08c8c8c8ch, 0a1a1a1a1h, 089898989h, 00d0d0d0dh,
    0bfbfbfbfh, 0e6e6e6e6h, 042424242h, 068686868h,
    041414141h, 099999999h, 02d2d2d2dh, 00f0f0f0fh,
    0b0b0b0b0h, 054545454h, 0bbbbbbbbh, 016161616h
    };
%ELSE
    Te4 : ARRAY [0..255] OF CARDINAL8 =
    {
    063h, 07ch, 077h, 07bh,
    0f2h, 06bh, 06fh, 0c5h,
    030h, 001h, 067h, 02bh,
    0feh, 0d7h, 0abh, 076h,
    0cah, 082h, 0c9h, 07dh,
    0fah, 059h, 047h, 0f0h,
    0adh, 0d4h, 0a2h, 0afh,
    09ch, 0a4h, 072h, 0c0h,
    0b7h, 0fdh, 093h, 026h,
    036h, 03fh, 0f7h, 0cch,
    034h, 0a5h, 0e5h, 0f1h,
    071h, 0d8h, 031h, 015h,
    004h, 0c7h, 023h, 0c3h,
    018h, 096h, 005h, 09ah,
    007h, 012h, 080h, 0e2h,
    0ebh, 027h, 0b2h, 075h,
    009h, 083h, 02ch, 01ah,
    01bh, 06eh, 05ah, 0a0h,
    052h, 03bh, 0d6h, 0b3h,
    029h, 0e3h, 02fh, 084h,
    053h, 0d1h, 000h, 0edh,
    020h, 0fch, 0b1h, 05bh,
    06ah, 0cbh, 0beh, 039h,
    04ah, 04ch, 058h, 0cfh,
    0d0h, 0efh, 0aah, 0fbh,
    043h, 04dh, 033h, 085h,
    045h, 0f9h, 002h, 07fh,
    050h, 03ch, 09fh, 0a8h,
    051h, 0a3h, 040h, 08fh,
    092h, 09dh, 038h, 0f5h,
    0bch, 0b6h, 0dah, 021h,
    010h, 0ffh, 0f3h, 0d2h,
    0cdh, 00ch, 013h, 0ech,
    05fh, 097h, 044h, 017h,
    0c4h, 0a7h, 07eh, 03dh,
    064h, 05dh, 019h, 073h,
    060h, 081h, 04fh, 0dch,
    022h, 02ah, 090h, 088h,
    046h, 0eeh, 0b8h, 014h,
    0deh, 05eh, 00bh, 0dbh,
    0e0h, 032h, 03ah, 00ah,
    049h, 006h, 024h, 05ch,
    0c2h, 0d3h, 0ach, 062h,
    091h, 095h, 0e4h, 079h,
    0e7h, 0c8h, 037h, 06dh,
    08dh, 0d5h, 04eh, 0a9h,
    06ch, 056h, 0f4h, 0eah,
    065h, 07ah, 0aeh, 008h,
    0bah, 078h, 025h, 02eh,
    01ch, 0a6h, 0b4h, 0c6h,
    0e8h, 0ddh, 074h, 01fh,
    04bh, 0bdh, 08bh, 08ah,
    070h, 03eh, 0b5h, 066h,
    048h, 003h, 0f6h, 00eh,
    061h, 035h, 057h, 0b9h,
    086h, 0c1h, 01dh, 09eh,
    0e1h, 0f8h, 098h, 011h,
    069h, 0d9h, 08eh, 094h,
    09bh, 01eh, 087h, 0e9h,
    0ceh, 055h, 028h, 0dfh,
    08ch, 0a1h, 089h, 00dh,
    0bfh, 0e6h, 042h, 068h,
    041h, 099h, 02dh, 00fh,
    0b0h, 054h, 0bbh, 016h
    };
%END

    Td0 : ARRAY [0..255] OF CARDINAL32 =
    {
    051f4a750h, 07e416553h, 01a17a4c3h, 03a275e96h,
    03bab6bcbh, 01f9d45f1h, 0acfa58abh, 04be30393h,
    02030fa55h, 0ad766df6h, 088cc7691h, 0f5024c25h,
    04fe5d7fch, 0c52acbd7h, 026354480h, 0b562a38fh,
    0deb15a49h, 025ba1b67h, 045ea0e98h, 05dfec0e1h,
    0c32f7502h, 0814cf012h, 08d4697a3h, 06bd3f9c6h,
    0038f5fe7h, 015929c95h, 0bf6d7aebh, 0955259dah,
    0d4be832dh, 0587421d3h, 049e06929h, 08ec9c844h,
    075c2896ah, 0f48e7978h, 099583e6bh, 027b971ddh,
    0bee14fb6h, 0f088ad17h, 0c920ac66h, 07dce3ab4h,
    063df4a18h, 0e51a3182h, 097513360h, 062537f45h,
    0b16477e0h, 0bb6bae84h, 0fe81a01ch, 0f9082b94h,
    070486858h, 08f45fd19h, 094de6c87h, 0527bf8b7h,
    0ab73d323h, 0724b02e2h, 0e31f8f57h, 06655ab2ah,
    0b2eb2807h, 02fb5c203h, 086c57b9ah, 0d33708a5h,
    0302887f2h, 023bfa5b2h, 002036abah, 0ed16825ch,
    08acf1c2bh, 0a779b492h, 0f307f2f0h, 04e69e2a1h,
    065daf4cdh, 00605bed5h, 0d134621fh, 0c4a6fe8ah,
    0342e539dh, 0a2f355a0h, 0058ae132h, 0a4f6eb75h,
    00b83ec39h, 04060efaah, 05e719f06h, 0bd6e1051h,
    03e218af9h, 096dd063dh, 0dd3e05aeh, 04de6bd46h,
    091548db5h, 071c45d05h, 00406d46fh, 0605015ffh,
    01998fb24h, 0d6bde997h, 0894043cch, 067d99e77h,
    0b0e842bdh, 007898b88h, 0e7195b38h, 079c8eedbh,
    0a17c0a47h, 07c420fe9h, 0f8841ec9h, 000000000h,
    009808683h, 0322bed48h, 01e1170ach, 06c5a724eh,
    0fd0efffbh, 00f853856h, 03daed51eh, 0362d3927h,
    00a0fd964h, 0685ca621h, 09b5b54d1h, 024362e3ah,
    00c0a67b1h, 09357e70fh, 0b4ee96d2h, 01b9b919eh,
    080c0c54fh, 061dc20a2h, 05a774b69h, 01c121a16h,
    0e293ba0ah, 0c0a02ae5h, 03c22e043h, 0121b171dh,
    00e090d0bh, 0f28bc7adh, 02db6a8b9h, 0141ea9c8h,
    057f11985h, 0af75074ch, 0ee99ddbbh, 0a37f60fdh,
    0f701269fh, 05c72f5bch, 044663bc5h, 05bfb7e34h,
    08b432976h, 0cb23c6dch, 0b6edfc68h, 0b8e4f163h,
    0d731dccah, 042638510h, 013972240h, 084c61120h,
    0854a247dh, 0d2bb3df8h, 0aef93211h, 0c729a16dh,
    01d9e2f4bh, 0dcb230f3h, 00d8652ech, 077c1e3d0h,
    02bb3166ch, 0a970b999h, 0119448fah, 047e96422h,
    0a8fc8cc4h, 0a0f03f1ah, 0567d2cd8h, 0223390efh,
    087494ec7h, 0d938d1c1h, 08ccaa2feh, 098d40b36h,
    0a6f581cfh, 0a57ade28h, 0dab78e26h, 03fadbfa4h,
    02c3a9de4h, 05078920dh, 06a5fcc9bh, 0547e4662h,
    0f68d13c2h, 090d8b8e8h, 02e39f75eh, 082c3aff5h,
    09f5d80beh, 069d0937ch, 06fd52da9h, 0cf2512b3h,
    0c8ac993bh, 010187da7h, 0e89c636eh, 0db3bbb7bh,
    0cd267809h, 06e5918f4h, 0ec9ab701h, 0834f9aa8h,
    0e6956e65h, 0aaffe67eh, 021bccf08h, 0ef15e8e6h,
    0bae79bd9h, 04a6f36ceh, 0ea9f09d4h, 029b07cd6h,
    031a4b2afh, 02a3f2331h, 0c6a59430h, 035a266c0h,
    0744ebc37h, 0fc82caa6h, 0e090d0b0h, 033a7d815h,
    0f104984ah, 041ecdaf7h, 07fcd500eh, 01791f62fh,
    0764dd68dh, 043efb04dh, 0ccaa4d54h, 0e49604dfh,
    09ed1b5e3h, 04c6a881bh, 0c12c1fb8h, 04665517fh,
    09d5eea04h, 0018c355dh, 0fa877473h, 0fb0b412eh,
    0b3671d5ah, 092dbd252h, 0e9105633h, 06dd64713h,
    09ad7618ch, 037a10c7ah, 059f8148eh, 0eb133c89h,
    0cea927eeh, 0b761c935h, 0e11ce5edh, 07a47b13ch,
    09cd2df59h, 055f2733fh, 01814ce79h, 073c737bfh,
    053f7cdeah, 05ffdaa5bh, 0df3d6f14h, 07844db86h,
    0caaff381h, 0b968c43eh, 03824342ch, 0c2a3405fh,
    0161dc372h, 0bce2250ch, 0283c498bh, 0ff0d9541h,
    039a80171h, 0080cb3deh, 0d8b4e49ch, 06456c190h,
    07bcb8461h, 0d532b670h, 0486c5c74h, 0d0b85742h
    };

    Td1 : ARRAY [0..255] OF CARDINAL32 =
    {
    05051f4a7h, 0537e4165h, 0c31a17a4h, 0963a275eh,
    0cb3bab6bh, 0f11f9d45h, 0abacfa58h, 0934be303h,
    0552030fah, 0f6ad766dh, 09188cc76h, 025f5024ch,
    0fc4fe5d7h, 0d7c52acbh, 080263544h, 08fb562a3h,
    049deb15ah, 06725ba1bh, 09845ea0eh, 0e15dfec0h,
    002c32f75h, 012814cf0h, 0a38d4697h, 0c66bd3f9h,
    0e7038f5fh, 09515929ch, 0ebbf6d7ah, 0da955259h,
    02dd4be83h, 0d3587421h, 02949e069h, 0448ec9c8h,
    06a75c289h, 078f48e79h, 06b99583eh, 0dd27b971h,
    0b6bee14fh, 017f088adh, 066c920ach, 0b47dce3ah,
    01863df4ah, 082e51a31h, 060975133h, 04562537fh,
    0e0b16477h, 084bb6baeh, 01cfe81a0h, 094f9082bh,
    058704868h, 0198f45fdh, 08794de6ch, 0b7527bf8h,
    023ab73d3h, 0e2724b02h, 057e31f8fh, 02a6655abh,
    007b2eb28h, 0032fb5c2h, 09a86c57bh, 0a5d33708h,
    0f2302887h, 0b223bfa5h, 0ba02036ah, 05ced1682h,
    02b8acf1ch, 092a779b4h, 0f0f307f2h, 0a14e69e2h,
    0cd65daf4h, 0d50605beh, 01fd13462h, 08ac4a6feh,
    09d342e53h, 0a0a2f355h, 032058ae1h, 075a4f6ebh,
    0390b83ech, 0aa4060efh, 0065e719fh, 051bd6e10h,
    0f93e218ah, 03d96dd06h, 0aedd3e05h, 0464de6bdh,
    0b591548dh, 00571c45dh, 06f0406d4h, 0ff605015h,
    0241998fbh, 097d6bde9h, 0cc894043h, 07767d99eh,
    0bdb0e842h, 08807898bh, 038e7195bh, 0db79c8eeh,
    047a17c0ah, 0e97c420fh, 0c9f8841eh, 000000000h,
    083098086h, 048322bedh, 0ac1e1170h, 04e6c5a72h,
    0fbfd0effh, 0560f8538h, 01e3daed5h, 027362d39h,
    0640a0fd9h, 021685ca6h, 0d19b5b54h, 03a24362eh,
    0b10c0a67h, 00f9357e7h, 0d2b4ee96h, 09e1b9b91h,
    04f80c0c5h, 0a261dc20h, 0695a774bh, 0161c121ah,
    00ae293bah, 0e5c0a02ah, 0433c22e0h, 01d121b17h,
    00b0e090dh, 0adf28bc7h, 0b92db6a8h, 0c8141ea9h,
    08557f119h, 04caf7507h, 0bbee99ddh, 0fda37f60h,
    09ff70126h, 0bc5c72f5h, 0c544663bh, 0345bfb7eh,
    0768b4329h, 0dccb23c6h, 068b6edfch, 063b8e4f1h,
    0cad731dch, 010426385h, 040139722h, 02084c611h,
    07d854a24h, 0f8d2bb3dh, 011aef932h, 06dc729a1h,
    04b1d9e2fh, 0f3dcb230h, 0ec0d8652h, 0d077c1e3h,
    06c2bb316h, 099a970b9h, 0fa119448h, 02247e964h,
    0c4a8fc8ch, 01aa0f03fh, 0d8567d2ch, 0ef223390h,
    0c787494eh, 0c1d938d1h, 0fe8ccaa2h, 03698d40bh,
    0cfa6f581h, 028a57adeh, 026dab78eh, 0a43fadbfh,
    0e42c3a9dh, 00d507892h, 09b6a5fcch, 062547e46h,
    0c2f68d13h, 0e890d8b8h, 05e2e39f7h, 0f582c3afh,
    0be9f5d80h, 07c69d093h, 0a96fd52dh, 0b3cf2512h,
    03bc8ac99h, 0a710187dh, 06ee89c63h, 07bdb3bbbh,
    009cd2678h, 0f46e5918h, 001ec9ab7h, 0a8834f9ah,
    065e6956eh, 07eaaffe6h, 00821bccfh, 0e6ef15e8h,
    0d9bae79bh, 0ce4a6f36h, 0d4ea9f09h, 0d629b07ch,
    0af31a4b2h, 0312a3f23h, 030c6a594h, 0c035a266h,
    037744ebch, 0a6fc82cah, 0b0e090d0h, 01533a7d8h,
    04af10498h, 0f741ecdah, 00e7fcd50h, 02f1791f6h,
    08d764dd6h, 04d43efb0h, 054ccaa4dh, 0dfe49604h,
    0e39ed1b5h, 01b4c6a88h, 0b8c12c1fh, 07f466551h,
    0049d5eeah, 05d018c35h, 073fa8774h, 02efb0b41h,
    05ab3671dh, 05292dbd2h, 033e91056h, 0136dd647h,
    08c9ad761h, 07a37a10ch, 08e59f814h, 089eb133ch,
    0eecea927h, 035b761c9h, 0ede11ce5h, 03c7a47b1h,
    0599cd2dfh, 03f55f273h, 0791814ceh, 0bf73c737h,
    0ea53f7cdh, 05b5ffdaah, 014df3d6fh, 0867844dbh,
    081caaff3h, 03eb968c4h, 02c382434h, 05fc2a340h,
    072161dc3h, 00cbce225h, 08b283c49h, 041ff0d95h,
    07139a801h, 0de080cb3h, 09cd8b4e4h, 0906456c1h,
    0617bcb84h, 070d532b6h, 074486c5ch, 042d0b857h
    };

    Td2 : ARRAY [0..255] OF CARDINAL32 =
    {
    0a75051f4h, 065537e41h, 0a4c31a17h, 05e963a27h,
    06bcb3babh, 045f11f9dh, 058abacfah, 003934be3h,
    0fa552030h, 06df6ad76h, 0769188cch, 04c25f502h,
    0d7fc4fe5h, 0cbd7c52ah, 044802635h, 0a38fb562h,
    05a49deb1h, 01b6725bah, 00e9845eah, 0c0e15dfeh,
    07502c32fh, 0f012814ch, 097a38d46h, 0f9c66bd3h,
    05fe7038fh, 09c951592h, 07aebbf6dh, 059da9552h,
    0832dd4beh, 021d35874h, 0692949e0h, 0c8448ec9h,
    0896a75c2h, 07978f48eh, 03e6b9958h, 071dd27b9h,
    04fb6bee1h, 0ad17f088h, 0ac66c920h, 03ab47dceh,
    04a1863dfh, 03182e51ah, 033609751h, 07f456253h,
    077e0b164h, 0ae84bb6bh, 0a01cfe81h, 02b94f908h,
    068587048h, 0fd198f45h, 06c8794deh, 0f8b7527bh,
    0d323ab73h, 002e2724bh, 08f57e31fh, 0ab2a6655h,
    02807b2ebh, 0c2032fb5h, 07b9a86c5h, 008a5d337h,
    087f23028h, 0a5b223bfh, 06aba0203h, 0825ced16h,
    01c2b8acfh, 0b492a779h, 0f2f0f307h, 0e2a14e69h,
    0f4cd65dah, 0bed50605h, 0621fd134h, 0fe8ac4a6h,
    0539d342eh, 055a0a2f3h, 0e132058ah, 0eb75a4f6h,
    0ec390b83h, 0efaa4060h, 09f065e71h, 01051bd6eh,
    08af93e21h, 0063d96ddh, 005aedd3eh, 0bd464de6h,
    08db59154h, 05d0571c4h, 0d46f0406h, 015ff6050h,
    0fb241998h, 0e997d6bdh, 043cc8940h, 09e7767d9h,
    042bdb0e8h, 08b880789h, 05b38e719h, 0eedb79c8h,
    00a47a17ch, 00fe97c42h, 01ec9f884h, 000000000h,
    086830980h, 0ed48322bh, 070ac1e11h, 0724e6c5ah,
    0fffbfd0eh, 038560f85h, 0d51e3daeh, 03927362dh,
    0d9640a0fh, 0a621685ch, 054d19b5bh, 02e3a2436h,
    067b10c0ah, 0e70f9357h, 096d2b4eeh, 0919e1b9bh,
    0c54f80c0h, 020a261dch, 04b695a77h, 01a161c12h,
    0ba0ae293h, 02ae5c0a0h, 0e0433c22h, 0171d121bh,
    00d0b0e09h, 0c7adf28bh, 0a8b92db6h, 0a9c8141eh,
    0198557f1h, 0074caf75h, 0ddbbee99h, 060fda37fh,
    0269ff701h, 0f5bc5c72h, 03bc54466h, 07e345bfbh,
    029768b43h, 0c6dccb23h, 0fc68b6edh, 0f163b8e4h,
    0dccad731h, 085104263h, 022401397h, 0112084c6h,
    0247d854ah, 03df8d2bbh, 03211aef9h, 0a16dc729h,
    02f4b1d9eh, 030f3dcb2h, 052ec0d86h, 0e3d077c1h,
    0166c2bb3h, 0b999a970h, 048fa1194h, 0642247e9h,
    08cc4a8fch, 03f1aa0f0h, 02cd8567dh, 090ef2233h,
    04ec78749h, 0d1c1d938h, 0a2fe8ccah, 00b3698d4h,
    081cfa6f5h, 0de28a57ah, 08e26dab7h, 0bfa43fadh,
    09de42c3ah, 0920d5078h, 0cc9b6a5fh, 04662547eh,
    013c2f68dh, 0b8e890d8h, 0f75e2e39h, 0aff582c3h,
    080be9f5dh, 0937c69d0h, 02da96fd5h, 012b3cf25h,
    0993bc8ach, 07da71018h, 0636ee89ch, 0bb7bdb3bh,
    07809cd26h, 018f46e59h, 0b701ec9ah, 09aa8834fh,
    06e65e695h, 0e67eaaffh, 0cf0821bch, 0e8e6ef15h,
    09bd9bae7h, 036ce4a6fh, 009d4ea9fh, 07cd629b0h,
    0b2af31a4h, 023312a3fh, 09430c6a5h, 066c035a2h,
    0bc37744eh, 0caa6fc82h, 0d0b0e090h, 0d81533a7h,
    0984af104h, 0daf741ech, 0500e7fcdh, 0f62f1791h,
    0d68d764dh, 0b04d43efh, 04d54ccaah, 004dfe496h,
    0b5e39ed1h, 0881b4c6ah, 01fb8c12ch, 0517f4665h,
    0ea049d5eh, 0355d018ch, 07473fa87h, 0412efb0bh,
    01d5ab367h, 0d25292dbh, 05633e910h, 047136dd6h,
    0618c9ad7h, 00c7a37a1h, 0148e59f8h, 03c89eb13h,
    027eecea9h, 0c935b761h, 0e5ede11ch, 0b13c7a47h,
    0df599cd2h, 0733f55f2h, 0ce791814h, 037bf73c7h,
    0cdea53f7h, 0aa5b5ffdh, 06f14df3dh, 0db867844h,
    0f381caafh, 0c43eb968h, 0342c3824h, 0405fc2a3h,
    0c372161dh, 0250cbce2h, 0498b283ch, 09541ff0dh,
    0017139a8h, 0b3de080ch, 0e49cd8b4h, 0c1906456h,
    084617bcbh, 0b670d532h, 05c74486ch, 05742d0b8h
    };

    Td3 : ARRAY [0..255] OF CARDINAL32 =
    {
    0f4a75051h, 04165537eh, 017a4c31ah, 0275e963ah,
    0ab6bcb3bh, 09d45f11fh, 0fa58abach, 0e303934bh,
    030fa5520h, 0766df6adh, 0cc769188h, 0024c25f5h,
    0e5d7fc4fh, 02acbd7c5h, 035448026h, 062a38fb5h,
    0b15a49deh, 0ba1b6725h, 0ea0e9845h, 0fec0e15dh,
    02f7502c3h, 04cf01281h, 04697a38dh, 0d3f9c66bh,
    08f5fe703h, 0929c9515h, 06d7aebbfh, 05259da95h,
    0be832dd4h, 07421d358h, 0e0692949h, 0c9c8448eh,
    0c2896a75h, 08e7978f4h, 0583e6b99h, 0b971dd27h,
    0e14fb6beh, 088ad17f0h, 020ac66c9h, 0ce3ab47dh,
    0df4a1863h, 01a3182e5h, 051336097h, 0537f4562h,
    06477e0b1h, 06bae84bbh, 081a01cfeh, 0082b94f9h,
    048685870h, 045fd198fh, 0de6c8794h, 07bf8b752h,
    073d323abh, 04b02e272h, 01f8f57e3h, 055ab2a66h,
    0eb2807b2h, 0b5c2032fh, 0c57b9a86h, 03708a5d3h,
    02887f230h, 0bfa5b223h, 0036aba02h, 016825cedh,
    0cf1c2b8ah, 079b492a7h, 007f2f0f3h, 069e2a14eh,
    0daf4cd65h, 005bed506h, 034621fd1h, 0a6fe8ac4h,
    02e539d34h, 0f355a0a2h, 08ae13205h, 0f6eb75a4h,
    083ec390bh, 060efaa40h, 0719f065eh, 06e1051bdh,
    0218af93eh, 0dd063d96h, 03e05aeddh, 0e6bd464dh,
    0548db591h, 0c45d0571h, 006d46f04h, 05015ff60h,
    098fb2419h, 0bde997d6h, 04043cc89h, 0d99e7767h,
    0e842bdb0h, 0898b8807h, 0195b38e7h, 0c8eedb79h,
    07c0a47a1h, 0420fe97ch, 0841ec9f8h, 000000000h,
    080868309h, 02bed4832h, 01170ac1eh, 05a724e6ch,
    00efffbfdh, 08538560fh, 0aed51e3dh, 02d392736h,
    00fd9640ah, 05ca62168h, 05b54d19bh, 0362e3a24h,
    00a67b10ch, 057e70f93h, 0ee96d2b4h, 09b919e1bh,
    0c0c54f80h, 0dc20a261h, 0774b695ah, 0121a161ch,
    093ba0ae2h, 0a02ae5c0h, 022e0433ch, 01b171d12h,
    0090d0b0eh, 08bc7adf2h, 0b6a8b92dh, 01ea9c814h,
    0f1198557h, 075074cafh, 099ddbbeeh, 07f60fda3h,
    001269ff7h, 072f5bc5ch, 0663bc544h, 0fb7e345bh,
    04329768bh, 023c6dccbh, 0edfc68b6h, 0e4f163b8h,
    031dccad7h, 063851042h, 097224013h, 0c6112084h,
    04a247d85h, 0bb3df8d2h, 0f93211aeh, 029a16dc7h,
    09e2f4b1dh, 0b230f3dch, 08652ec0dh, 0c1e3d077h,
    0b3166c2bh, 070b999a9h, 09448fa11h, 0e9642247h,
    0fc8cc4a8h, 0f03f1aa0h, 07d2cd856h, 03390ef22h,
    0494ec787h, 038d1c1d9h, 0caa2fe8ch, 0d40b3698h,
    0f581cfa6h, 07ade28a5h, 0b78e26dah, 0adbfa43fh,
    03a9de42ch, 078920d50h, 05fcc9b6ah, 07e466254h,
    08d13c2f6h, 0d8b8e890h, 039f75e2eh, 0c3aff582h,
    05d80be9fh, 0d0937c69h, 0d52da96fh, 02512b3cfh,
    0ac993bc8h, 0187da710h, 09c636ee8h, 03bbb7bdbh,
    0267809cdh, 05918f46eh, 09ab701ech, 04f9aa883h,
    0956e65e6h, 0ffe67eaah, 0bccf0821h, 015e8e6efh,
    0e79bd9bah, 06f36ce4ah, 09f09d4eah, 0b07cd629h,
    0a4b2af31h, 03f23312ah, 0a59430c6h, 0a266c035h,
    04ebc3774h, 082caa6fch, 090d0b0e0h, 0a7d81533h,
    004984af1h, 0ecdaf741h, 0cd500e7fh, 091f62f17h,
    04dd68d76h, 0efb04d43h, 0aa4d54cch, 09604dfe4h,
    0d1b5e39eh, 06a881b4ch, 02c1fb8c1h, 065517f46h,
    05eea049dh, 08c355d01h, 0877473fah, 00b412efbh,
    0671d5ab3h, 0dbd25292h, 0105633e9h, 0d647136dh,
    0d7618c9ah, 0a10c7a37h, 0f8148e59h, 0133c89ebh,
    0a927eeceh, 061c935b7h, 01ce5ede1h, 047b13c7ah,
    0d2df599ch, 0f2733f55h, 014ce7918h, 0c737bf73h,
    0f7cdea53h, 0fdaa5b5fh, 03d6f14dfh, 044db8678h,
    0aff381cah, 068c43eb9h, 024342c38h, 0a3405fc2h,
    01dc37216h, 0e2250cbch, 03c498b28h, 00d9541ffh,
    0a8017139h, 00cb3de08h, 0b4e49cd8h, 056c19064h,
    0cb84617bh, 032b670d5h, 06c5c7448h, 0b85742d0h
    };

%IF BIGTABLE %THEN
    Td4 : ARRAY [0..255] OF CARDINAL32 =
    {
    052525252h, 009090909h, 06a6a6a6ah, 0d5d5d5d5h,
    030303030h, 036363636h, 0a5a5a5a5h, 038383838h,
    0bfbfbfbfh, 040404040h, 0a3a3a3a3h, 09e9e9e9eh,
    081818181h, 0f3f3f3f3h, 0d7d7d7d7h, 0fbfbfbfbh,
    07c7c7c7ch, 0e3e3e3e3h, 039393939h, 082828282h,
    09b9b9b9bh, 02f2f2f2fh, 0ffffffffh, 087878787h,
    034343434h, 08e8e8e8eh, 043434343h, 044444444h,
    0c4c4c4c4h, 0dedededeh, 0e9e9e9e9h, 0cbcbcbcbh,
    054545454h, 07b7b7b7bh, 094949494h, 032323232h,
    0a6a6a6a6h, 0c2c2c2c2h, 023232323h, 03d3d3d3dh,
    0eeeeeeeeh, 04c4c4c4ch, 095959595h, 00b0b0b0bh,
    042424242h, 0fafafafah, 0c3c3c3c3h, 04e4e4e4eh,
    008080808h, 02e2e2e2eh, 0a1a1a1a1h, 066666666h,
    028282828h, 0d9d9d9d9h, 024242424h, 0b2b2b2b2h,
    076767676h, 05b5b5b5bh, 0a2a2a2a2h, 049494949h,
    06d6d6d6dh, 08b8b8b8bh, 0d1d1d1d1h, 025252525h,
    072727272h, 0f8f8f8f8h, 0f6f6f6f6h, 064646464h,
    086868686h, 068686868h, 098989898h, 016161616h,
    0d4d4d4d4h, 0a4a4a4a4h, 05c5c5c5ch, 0cccccccch,
    05d5d5d5dh, 065656565h, 0b6b6b6b6h, 092929292h,
    06c6c6c6ch, 070707070h, 048484848h, 050505050h,
    0fdfdfdfdh, 0ededededh, 0b9b9b9b9h, 0dadadadah,
    05e5e5e5eh, 015151515h, 046464646h, 057575757h,
    0a7a7a7a7h, 08d8d8d8dh, 09d9d9d9dh, 084848484h,
    090909090h, 0d8d8d8d8h, 0ababababh, 000000000h,
    08c8c8c8ch, 0bcbcbcbch, 0d3d3d3d3h, 00a0a0a0ah,
    0f7f7f7f7h, 0e4e4e4e4h, 058585858h, 005050505h,
    0b8b8b8b8h, 0b3b3b3b3h, 045454545h, 006060606h,
    0d0d0d0d0h, 02c2c2c2ch, 01e1e1e1eh, 08f8f8f8fh,
    0cacacacah, 03f3f3f3fh, 00f0f0f0fh, 002020202h,
    0c1c1c1c1h, 0afafafafh, 0bdbdbdbdh, 003030303h,
    001010101h, 013131313h, 08a8a8a8ah, 06b6b6b6bh,
    03a3a3a3ah, 091919191h, 011111111h, 041414141h,
    04f4f4f4fh, 067676767h, 0dcdcdcdch, 0eaeaeaeah,
    097979797h, 0f2f2f2f2h, 0cfcfcfcfh, 0cecececeh,
    0f0f0f0f0h, 0b4b4b4b4h, 0e6e6e6e6h, 073737373h,
    096969696h, 0acacacach, 074747474h, 022222222h,
    0e7e7e7e7h, 0adadadadh, 035353535h, 085858585h,
    0e2e2e2e2h, 0f9f9f9f9h, 037373737h, 0e8e8e8e8h,
    01c1c1c1ch, 075757575h, 0dfdfdfdfh, 06e6e6e6eh,
    047474747h, 0f1f1f1f1h, 01a1a1a1ah, 071717171h,
    01d1d1d1dh, 029292929h, 0c5c5c5c5h, 089898989h,
    06f6f6f6fh, 0b7b7b7b7h, 062626262h, 00e0e0e0eh,
    0aaaaaaaah, 018181818h, 0bebebebeh, 01b1b1b1bh,
    0fcfcfcfch, 056565656h, 03e3e3e3eh, 04b4b4b4bh,
    0c6c6c6c6h, 0d2d2d2d2h, 079797979h, 020202020h,
    09a9a9a9ah, 0dbdbdbdbh, 0c0c0c0c0h, 0fefefefeh,
    078787878h, 0cdcdcdcdh, 05a5a5a5ah, 0f4f4f4f4h,
    01f1f1f1fh, 0ddddddddh, 0a8a8a8a8h, 033333333h,
    088888888h, 007070707h, 0c7c7c7c7h, 031313131h,
    0b1b1b1b1h, 012121212h, 010101010h, 059595959h,
    027272727h, 080808080h, 0ecececech, 05f5f5f5fh,
    060606060h, 051515151h, 07f7f7f7fh, 0a9a9a9a9h,
    019191919h, 0b5b5b5b5h, 04a4a4a4ah, 00d0d0d0dh,
    02d2d2d2dh, 0e5e5e5e5h, 07a7a7a7ah, 09f9f9f9fh,
    093939393h, 0c9c9c9c9h, 09c9c9c9ch, 0efefefefh,
    0a0a0a0a0h, 0e0e0e0e0h, 03b3b3b3bh, 04d4d4d4dh,
    0aeaeaeaeh, 02a2a2a2ah, 0f5f5f5f5h, 0b0b0b0b0h,
    0c8c8c8c8h, 0ebebebebh, 0bbbbbbbbh, 03c3c3c3ch,
    083838383h, 053535353h, 099999999h, 061616161h,
    017171717h, 02b2b2b2bh, 004040404h, 07e7e7e7eh,
    0babababah, 077777777h, 0d6d6d6d6h, 026262626h,
    0e1e1e1e1h, 069696969h, 014141414h, 063636363h,
    055555555h, 021212121h, 00c0c0c0ch, 07d7d7d7dh
    };
%ELSE
    Td4 : ARRAY [0..255] OF CARDINAL8 =
    {
    052h, 009h, 06ah, 0d5h,
    030h, 036h, 0a5h, 038h,
    0bfh, 040h, 0a3h, 09eh,
    081h, 0f3h, 0d7h, 0fbh,
    07ch, 0e3h, 039h, 082h,
    09bh, 02fh, 0ffh, 087h,
    034h, 08eh, 043h, 044h,
    0c4h, 0deh, 0e9h, 0cbh,
    054h, 07bh, 094h, 032h,
    0a6h, 0c2h, 023h, 03dh,
    0eeh, 04ch, 095h, 00bh,
    042h, 0fah, 0c3h, 04eh,
    008h, 02eh, 0a1h, 066h,
    028h, 0d9h, 024h, 0b2h,
    076h, 05bh, 0a2h, 049h,
    06dh, 08bh, 0d1h, 025h,
    072h, 0f8h, 0f6h, 064h,
    086h, 068h, 098h, 016h,
    0d4h, 0a4h, 05ch, 0cch,
    05dh, 065h, 0b6h, 092h,
    06ch, 070h, 048h, 050h,
    0fdh, 0edh, 0b9h, 0dah,
    05eh, 015h, 046h, 057h,
    0a7h, 08dh, 09dh, 084h,
    090h, 0d8h, 0abh, 000h,
    08ch, 0bch, 0d3h, 00ah,
    0f7h, 0e4h, 058h, 005h,
    0b8h, 0b3h, 045h, 006h,
    0d0h, 02ch, 01eh, 08fh,
    0cah, 03fh, 00fh, 002h,
    0c1h, 0afh, 0bdh, 003h,
    001h, 013h, 08ah, 06bh,
    03ah, 091h, 011h, 041h,
    04fh, 067h, 0dch, 0eah,
    097h, 0f2h, 0cfh, 0ceh,
    0f0h, 0b4h, 0e6h, 073h,
    096h, 0ach, 074h, 022h,
    0e7h, 0adh, 035h, 085h,
    0e2h, 0f9h, 037h, 0e8h,
    01ch, 075h, 0dfh, 06eh,
    047h, 0f1h, 01ah, 071h,
    01dh, 029h, 0c5h, 089h,
    06fh, 0b7h, 062h, 00eh,
    0aah, 018h, 0beh, 01bh,
    0fch, 056h, 03eh, 04bh,
    0c6h, 0d2h, 079h, 020h,
    09ah, 0dbh, 0c0h, 0feh,
    078h, 0cdh, 05ah, 0f4h,
    01fh, 0ddh, 0a8h, 033h,
    088h, 007h, 0c7h, 031h,
    0b1h, 012h, 010h, 059h,
    027h, 080h, 0ech, 05fh,
    060h, 051h, 07fh, 0a9h,
    019h, 0b5h, 04ah, 00dh,
    02dh, 0e5h, 07ah, 09fh,
    093h, 0c9h, 09ch, 0efh,
    0a0h, 0e0h, 03bh, 04dh,
    0aeh, 02ah, 0f5h, 0b0h,
    0c8h, 0ebh, 0bbh, 03ch,
    083h, 053h, 099h, 061h,
    017h, 02bh, 004h, 07eh,
    0bah, 077h, 0d6h, 026h,
    0e1h, 069h, 014h, 063h,
    055h, 021h, 00ch, 07dh
    };
%END

PROCEDURE KeySetup(crypt : AES; key : ARRAY OF BYTE; keySize : CARDINAL);
VAR
    temp        : CARDINAL32;
    i, j        : ADRCARD;
    rounds      : ADRCARD;
    rk          : POINTER TO ARRAY [0..15] OF CARDINAL32;
BEGIN
    rk := ADR(crypt^.rke);

    IF keySize = 128 THEN
        crypt^.rounds := 10;

        (* must assume byte alignment for key array *)

        crypt^.rke:Key128 := key;

        BIGENDIAN(crypt^.rke[0]);
        BIGENDIAN(crypt^.rke[1]);
        BIGENDIAN(crypt^.rke[2]);
        BIGENDIAN(crypt^.rke[3]);

        FOR i := 0 TO 9 DO
            temp := rk^[3];

            %IF BIGTABLE %THEN
                rk^[4] := rk^[0] BXOR
                          (Te4[(temp SHR 16) BAND 0ffh] BAND 0ff000000h) BXOR
                          (Te4[(temp SHR  8) BAND 0ffh] BAND 000ff0000h) BXOR
                          (Te4[(temp       ) BAND 0ffh] BAND 00000ff00h) BXOR
                          (Te4[(temp SHR 24)          ] BAND 0000000ffh) BXOR
                          rcon[i];
            %ELSE
                rk^[4] := rk^[0] BXOR
                          (ORD(Te4[(temp SHR 16) BAND 0ffh]) SHL 24) BXOR
                          (ORD(Te4[(temp SHR  8) BAND 0ffh]) SHL 16) BXOR
                          (ORD(Te4[(temp       ) BAND 0ffh]) SHL  8) BXOR
                          (ORD(Te4[(temp SHR 24)          ])       ) BXOR
                          rcon[i];
            %END
            rk^[5] := rk^[1] BXOR rk^[4];
            rk^[6] := rk^[2] BXOR rk^[5];
            rk^[7] := rk^[3] BXOR rk^[6];

            rk := ADDADR(rk, 4*4);
        END;

    ELSIF keySize = 192 THEN
        crypt^.rounds := 12;

        (* must assume byte alignment for key array *)

        crypt^.rke:Key192 := key;

        BIGENDIAN(crypt^.rke[0]);
        BIGENDIAN(crypt^.rke[1]);
        BIGENDIAN(crypt^.rke[2]);
        BIGENDIAN(crypt^.rke[3]);
        BIGENDIAN(crypt^.rke[4]);
        BIGENDIAN(crypt^.rke[5]);

        i := 0;
        LOOP
            temp := rk^[5];

            %IF BIGTABLE %THEN
                rk^[6] := rk^[0] BXOR
                          (Te4[(temp SHR 16) BAND 0ffh] BAND 0ff000000h) BXOR
                          (Te4[(temp SHR  8) BAND 0ffh] BAND 000ff0000h) BXOR
                          (Te4[(temp       ) BAND 0ffh] BAND 00000ff00h) BXOR
                          (Te4[(temp SHR 24)          ] BAND 0000000ffh) BXOR
                          rcon[i];
            %ELSE
                rk^[6] := rk^[0] BXOR
                          (ORD(Te4[(temp SHR 16) BAND 0ffh]) SHL 24) BXOR
                          (ORD(Te4[(temp SHR  8) BAND 0ffh]) SHL 16) BXOR
                          (ORD(Te4[(temp       ) BAND 0ffh]) SHL  8) BXOR
                          (ORD(Te4[(temp SHR 24)          ])       ) BXOR
                          rcon[i];
            %END
            rk^[7] := rk^[1] BXOR rk^[6];
            rk^[8] := rk^[2] BXOR rk^[7];
            rk^[9] := rk^[3] BXOR rk^[8];

            INC(i);
            IF i = 8 THEN
                EXIT;
            END;

            rk^[10] := rk^[4] BXOR rk^[9];
            rk^[11] := rk^[5] BXOR rk^[10];

            rk := ADDADR(rk, 6*4);
        END;

    ELSE(*keySize = 256*)
        crypt^.rounds := 14;

        (* must assume byte alignment for key array *)

        crypt^.rke:Key256 := key;

        BIGENDIAN(crypt^.rke[0]);
        BIGENDIAN(crypt^.rke[1]);
        BIGENDIAN(crypt^.rke[2]);
        BIGENDIAN(crypt^.rke[3]);
        BIGENDIAN(crypt^.rke[4]);
        BIGENDIAN(crypt^.rke[5]);
        BIGENDIAN(crypt^.rke[6]);
        BIGENDIAN(crypt^.rke[7]);

        i := 0;
        LOOP
            temp := rk^[7];

            %IF BIGTABLE %THEN
                rk^[8] := rk^[0] BXOR
                          (Te4[(temp SHR 16) BAND 0ffh] BAND 0ff000000h) BXOR
                          (Te4[(temp SHR  8) BAND 0ffh] BAND 000ff0000h) BXOR
                          (Te4[(temp       ) BAND 0ffh] BAND 00000ff00h) BXOR
                          (Te4[(temp SHR 24)          ] BAND 0000000ffh) BXOR
                          rcon[i];
            %ELSE
                rk^[8] := rk^[0] BXOR
                          (ORD(Te4[(temp SHR 16) BAND 0ffh]) SHL 24) BXOR
                          (ORD(Te4[(temp SHR  8) BAND 0ffh]) SHL 16) BXOR
                          (ORD(Te4[(temp       ) BAND 0ffh]) SHL  8) BXOR
                          (ORD(Te4[(temp SHR 24)          ])       ) BXOR
                          rcon[i];
            %END
            rk^[9] :=  rk^[1] BXOR rk^[8];
            rk^[10] := rk^[2] BXOR rk^[9];
            rk^[11] := rk^[3] BXOR rk^[10];

            INC(i);
            IF i = 7 THEN
                EXIT;
            END;

            temp := rk^[11];
            %IF BIGTABLE %THEN
                rk^[12] := rk^[4] BXOR
                           (Te4[(temp SHR 24)          ] BAND 0ff000000h) BXOR
                           (Te4[(temp SHR 16) BAND 0ffh] BAND 000ff0000h) BXOR
                           (Te4[(temp SHR  8) BAND 0ffh] BAND 00000ff00h) BXOR
                           (Te4[(temp       ) BAND 0ffh] BAND 0000000ffh);
            %ELSE
                rk^[12] := rk^[4] BXOR
                           (ORD(Te4[(temp SHR 24)          ]) SHL 24) BXOR
                           (ORD(Te4[(temp SHR 16) BAND 0ffh]) SHL 16) BXOR
                           (ORD(Te4[(temp SHR  8) BAND 0ffh]) SHL  8) BXOR
                           (ORD(Te4[(temp       ) BAND 0ffh])       );
            %END
            rk^[13] := rk^[5] BXOR rk^[12];
            rk^[14] := rk^[6] BXOR rk^[13];
            rk^[15] := rk^[7] BXOR rk^[14];

            rk := ADDADR(rk, 8*4);
        END;
    END;

    (* make the decrypt key *)

    crypt^.rkd := crypt^.rke;
    rk := ADR(crypt^.rkd);

    (* invert the order of the round keys: *)

    i := 0;
    j := crypt^.rounds * 4;
    WHILE i < j DO
        temp := rk^[i];
        rk^[i] := rk^[j];
        rk^[j] := temp;

        temp := rk^[i+1];
        rk^[i+1] := rk^[j+1];
        rk^[j+1] := temp;

        temp := rk^[i+2];
        rk^[i+2] := rk^[j+2];
        rk^[j+2] := temp;

        temp := rk^[i+3];
        rk^[i+3] := rk^[j+3];
        rk^[j+3] := temp;

        i := i + 4;
        j := j - 4;
    END;

    (* apply the inverse MixColumn transform to all round keys
       but the first and the last *)

    rk := ADDADR(rk, 4*4);
    rounds := crypt^.rounds;
    FOR i := 1 TO rounds-1 DO
        %IF BIGTABLE %THEN
            rk^[0] := Td0[Te4[(rk^[0] SHR 24)          ] BAND 0FFh] BXOR
                      Td1[Te4[(rk^[0] SHR 16) BAND 0FFh] BAND 0FFh] BXOR
                      Td2[Te4[(rk^[0] SHR  8) BAND 0FFh] BAND 0FFh] BXOR
                      Td3[Te4[(rk^[0]       ) BAND 0FFh] BAND 0FFh];

            rk^[1] := Td0[Te4[(rk^[1] SHR 24)          ] BAND 0FFh] BXOR
                      Td1[Te4[(rk^[1] SHR 16) BAND 0FFh] BAND 0FFh] BXOR
                      Td2[Te4[(rk^[1] SHR  8) BAND 0FFh] BAND 0FFh] BXOR
                      Td3[Te4[(rk^[1]       ) BAND 0FFh] BAND 0FFh];

            rk^[2] := Td0[Te4[(rk^[2] SHR 24)          ] BAND 0FFh] BXOR
                      Td1[Te4[(rk^[2] SHR 16) BAND 0FFh] BAND 0FFh] BXOR
                      Td2[Te4[(rk^[2] SHR  8) BAND 0FFh] BAND 0FFh] BXOR
                      Td3[Te4[(rk^[2]       ) BAND 0FFh] BAND 0FFh];

            rk^[3] := Td0[Te4[(rk^[3] SHR 24)          ] BAND 0FFh] BXOR
                      Td1[Te4[(rk^[3] SHR 16) BAND 0FFh] BAND 0FFh] BXOR
                      Td2[Te4[(rk^[3] SHR  8) BAND 0FFh] BAND 0FFh] BXOR
                      Td3[Te4[(rk^[3]       ) BAND 0FFh] BAND 0FFh];
        %ELSE
            rk^[0] := Td0[Te4[(rk^[0] SHR 24)          ]] BXOR
                      Td1[Te4[(rk^[0] SHR 16) BAND 0FFh]] BXOR
                      Td2[Te4[(rk^[0] SHR  8) BAND 0FFh]] BXOR
                      Td3[Te4[(rk^[0]       ) BAND 0FFh]];

            rk^[1] := Td0[Te4[(rk^[1] SHR 24)          ]] BXOR
                      Td1[Te4[(rk^[1] SHR 16) BAND 0FFh]] BXOR
                      Td2[Te4[(rk^[1] SHR  8) BAND 0FFh]] BXOR
                      Td3[Te4[(rk^[1]       ) BAND 0FFh]];

            rk^[2] := Td0[Te4[(rk^[2] SHR 24)          ]] BXOR
                      Td1[Te4[(rk^[2] SHR 16) BAND 0FFh]] BXOR
                      Td2[Te4[(rk^[2] SHR  8) BAND 0FFh]] BXOR
                      Td3[Te4[(rk^[2]       ) BAND 0FFh]];

            rk^[3] := Td0[Te4[(rk^[3] SHR 24)          ]] BXOR
                      Td1[Te4[(rk^[3] SHR 16) BAND 0FFh]] BXOR
                      Td2[Te4[(rk^[3] SHR  8) BAND 0FFh]] BXOR
                      Td3[Te4[(rk^[3]       ) BAND 0FFh]];
        %END

        rk := ADDADR(rk, 4*4);
    END;
END KeySetup;

PROCEDURE Create(key : ARRAY OF BYTE; keySize : CARDINAL) : AES;
VAR
    crypt       : AES;
BEGIN
    crypt := NIL;

    IF (keySize = 128) OR (keySize = 192) OR (keySize = 256) THEN
        NEW(crypt);
        crypt^.heap := GetHeap();

        KeySetup(crypt, key, keySize);
    END;

    RETURN crypt;
END Create;

PROCEDURE Destroy(VAR INOUT crypt : AES);
BEGIN
    DeallocateEx(crypt, SIZE(crypt^), crypt^.heap);
END Destroy;

PROCEDURE ResetIV(crypt : AES; iv : IV);
BEGIN
    crypt^.iv := iv;
END ResetIV;

PROCEDURE encrypt(crypt : AES; VAR INOUT data : DataBlock);
VAR
    r                   : CARDINAL;
    s0, s1, s2, s3      : CARDINAL32;
    t0, t1, t2, t3      : CARDINAL32;
    rk                  : POINTER TO ARRAY [0..7] OF CARDINAL32;
BEGIN
    rk := ADR(crypt^.rke);

    s0 := data[0] BXOR rk^[0];
    s1 := data[1] BXOR rk^[1];
    s2 := data[2] BXOR rk^[2];
    s3 := data[3] BXOR rk^[3];

    (* Nr - 1 full rounds *)

    r := crypt^.rounds / 2;
    LOOP
        DEC(r);

        t0 := Te0[(s0 SHR 24)          ] BXOR
              Te1[(s1 SHR 16) BAND 0FFh] BXOR
              Te2[(s2 SHR  8) BAND 0FFh] BXOR
              Te3[(s3       ) BAND 0FFh] BXOR
              rk^[4];

        t1 := Te0[(s1 SHR 24)          ] BXOR
              Te1[(s2 SHR 16) BAND 0FFh] BXOR
              Te2[(s3 SHR  8) BAND 0FFh] BXOR
              Te3[(s0       ) BAND 0FFh] BXOR
              rk^[5];

        t2 := Te0[(s2 SHR 24)          ] BXOR
              Te1[(s3 SHR 16) BAND 0FFh] BXOR
              Te2[(s0 SHR  8) BAND 0FFh] BXOR
              Te3[(s1       ) BAND 0FFh] BXOR
              rk^[6];

        t3 := Te0[(s3 SHR 24)          ] BXOR
              Te1[(s0 SHR 16) BAND 0FFh] BXOR
              Te2[(s1 SHR  8) BAND 0FFh] BXOR
              Te3[(s2       ) BAND 0FFh] BXOR
              rk^[7];

        rk := ADDADR(rk, 8*4);

        IF r <> 0 THEN
            s0 := Te0[(t0 SHR 24)          ] BXOR
                  Te1[(t1 SHR 16) BAND 0FFh] BXOR
                  Te2[(t2 SHR  8) BAND 0FFh] BXOR
                  Te3[(t3       ) BAND 0FFh] BXOR
                  rk^[0];

            s1 := Te0[(t1 SHR 24)          ] BXOR
                  Te1[(t2 SHR 16) BAND 0FFh] BXOR
                  Te2[(t3 SHR  8) BAND 0FFh] BXOR
                  Te3[(t0       ) BAND 0FFh] BXOR
                  rk^[1];

            s2 := Te0[(t2 SHR 24)          ] BXOR
                  Te1[(t3 SHR 16) BAND 0FFh] BXOR
                  Te2[(t0 SHR  8) BAND 0FFh] BXOR
                  Te3[(t1       ) BAND 0FFh] BXOR
                  rk^[2];

            s3 := Te0[(t3 SHR 24)          ] BXOR
                  Te1[(t0 SHR 16) BAND 0FFh] BXOR
                  Te2[(t1 SHR  8) BAND 0FFh] BXOR
                  Te3[(t2       ) BAND 0FFh] BXOR
                  rk^[3];
        ELSE
            EXIT;
        END;
    END;

    (* apply last round *)

    %IF BIGTABLE %THEN
        data[0] := (Te4[(t0 SHR 24)          ] BAND 0ff000000h) BXOR
                   (Te4[(t1 SHR 16) BAND 0FFh] BAND 000ff0000h) BXOR
                   (Te4[(t2 SHR  8) BAND 0FFh] BAND 00000ff00h) BXOR
                   (Te4[(t3       ) BAND 0FFh] BAND 0000000ffh) BXOR
                   rk^[0];

        data[1] := (Te4[(t1 SHR 24)          ] BAND 0ff000000h) BXOR
                   (Te4[(t2 SHR 16) BAND 0FFh] BAND 000ff0000h) BXOR
                   (Te4[(t3 SHR  8) BAND 0FFh] BAND 00000ff00h) BXOR
                   (Te4[(t0       ) BAND 0FFh] BAND 0000000ffh) BXOR
                   rk^[1];

        data[2] := (Te4[(t2 SHR 24)          ] BAND 0ff000000h) BXOR
                   (Te4[(t3 SHR 16) BAND 0FFh] BAND 000ff0000h) BXOR
                   (Te4[(t0 SHR  8) BAND 0FFh] BAND 00000ff00h) BXOR
                   (Te4[(t1       ) BAND 0FFh] BAND 0000000ffh) BXOR
                   rk^[2];

        data[3] := (Te4[(t3 SHR 24)          ] BAND 0ff000000h) BXOR
                   (Te4[(t0 SHR 16) BAND 0FFh] BAND 000ff0000h) BXOR
                   (Te4[(t1 SHR  8) BAND 0FFh] BAND 00000ff00h) BXOR
                   (Te4[(t2       ) BAND 0FFh] BAND 0000000ffh) BXOR
                   rk^[3];
    %ELSE
        data[0] := (ORD(Te4[(t0 SHR 24)          ]) SHL 24) BXOR
                   (ORD(Te4[(t1 SHR 16) BAND 0FFh]) SHL 16) BXOR
                   (ORD(Te4[(t2 SHR  8) BAND 0FFh]) SHL  8) BXOR
                   (ORD(Te4[(t3       ) BAND 0FFh])       ) BXOR
                   rk^[0];

        data[1] := (ORD(Te4[(t1 SHR 24)          ]) SHL 24) BXOR
                   (ORD(Te4[(t2 SHR 16) BAND 0FFh]) SHL 16) BXOR
                   (ORD(Te4[(t3 SHR  8) BAND 0FFh]) SHL  8) BXOR
                   (ORD(Te4[(t0       ) BAND 0FFh])       ) BXOR
                   rk^[1];

        data[2] := (ORD(Te4[(t2 SHR 24)          ]) SHL 24) BXOR
                   (ORD(Te4[(t3 SHR 16) BAND 0FFh]) SHL 16) BXOR
                   (ORD(Te4[(t0 SHR  8) BAND 0FFh]) SHL  8) BXOR
                   (ORD(Te4[(t1       ) BAND 0FFh])       ) BXOR
                   rk^[2];

        data[3] := (ORD(Te4[(t3 SHR 24)          ]) SHL 24) BXOR
                   (ORD(Te4[(t0 SHR 16) BAND 0FFh]) SHL 16) BXOR
                   (ORD(Te4[(t1 SHR  8) BAND 0FFh]) SHL  8) BXOR
                   (ORD(Te4[(t2       ) BAND 0FFh])       ) BXOR
                   rk^[3];
    %END
END encrypt;

PROCEDURE decrypt(crypt : AES; VAR INOUT data : DataBlock);
VAR
    r                   : CARDINAL;
    s0, s1, s2, s3      : CARDINAL32;
    t0, t1, t2, t3      : CARDINAL32;
    rk                  : POINTER TO ARRAY [0..7] OF CARDINAL32;
BEGIN
    rk := ADR(crypt^.rkd);

    s0 := data[0] BXOR rk^[0];
    s1 := data[1] BXOR rk^[1];
    s2 := data[2] BXOR rk^[2];
    s3 := data[3] BXOR rk^[3];

    (* Nr - 1 full rounds *)

    r := crypt^.rounds / 2;
    LOOP
        DEC(r);

        t0 := Td0[(s0 SHR 24)          ] BXOR
              Td1[(s3 SHR 16) BAND 0FFh] BXOR
              Td2[(s2 SHR  8) BAND 0FFh] BXOR
              Td3[(s1       ) BAND 0FFh] BXOR
              rk^[4];

        t1 := Td0[(s1 SHR 24)          ] BXOR
              Td1[(s0 SHR 16) BAND 0FFh] BXOR
              Td2[(s3 SHR  8) BAND 0FFh] BXOR
              Td3[(s2       ) BAND 0FFh] BXOR
              rk^[5];

        t2 := Td0[(s2 SHR 24)          ] BXOR
              Td1[(s1 SHR 16) BAND 0FFh] BXOR
              Td2[(s0 SHR  8) BAND 0FFh] BXOR
              Td3[(s3       ) BAND 0FFh] BXOR
              rk^[6];

        t3 := Td0[(s3 SHR 24)          ] BXOR
              Td1[(s2 SHR 16) BAND 0FFh] BXOR
              Td2[(s1 SHR  8) BAND 0FFh] BXOR
              Td3[(s0       ) BAND 0FFh] BXOR
              rk^[7];

        rk := ADDADR(rk, 8*4);

        IF r <> 0 THEN
            s0 := Td0[(t0 SHR 24)          ] BXOR
                  Td1[(t3 SHR 16) BAND 0FFh] BXOR
                  Td2[(t2 SHR  8) BAND 0FFh] BXOR
                  Td3[(t1       ) BAND 0FFh] BXOR
                  rk^[0];

            s1 := Td0[(t1 SHR 24)          ] BXOR
                  Td1[(t0 SHR 16) BAND 0FFh] BXOR
                  Td2[(t3 SHR  8) BAND 0FFh] BXOR
                  Td3[(t2       ) BAND 0FFh] BXOR
                  rk^[1];

            s2 := Td0[(t2 SHR 24)          ] BXOR
                  Td1[(t1 SHR 16) BAND 0FFh] BXOR
                  Td2[(t0 SHR  8) BAND 0FFh] BXOR
                  Td3[(t3       ) BAND 0FFh] BXOR
                  rk^[2];

            s3 := Td0[(t3 SHR 24)          ] BXOR
                  Td1[(t2 SHR 16) BAND 0FFh] BXOR
                  Td2[(t1 SHR  8) BAND 0FFh] BXOR
                  Td3[(t0       ) BAND 0FFh] BXOR
                  rk^[3];
        ELSE
            EXIT;
        END;
    END;

    (* last round *)

    %IF BIGTABLE %THEN
        data[0] := (Td4[(t0 SHR 24)          ] BAND 0ff000000h) BXOR
                   (Td4[(t3 SHR 16) BAND 0FFh] BAND 000ff0000h) BXOR
                   (Td4[(t2 SHR  8) BAND 0FFh] BAND 00000ff00h) BXOR
                   (Td4[(t1       ) BAND 0FFh] BAND 0000000ffh) BXOR
                   rk^[0];

        data[1] := (Td4[(t1 SHR 24)          ] BAND 0ff000000h) BXOR
                   (Td4[(t0 SHR 16) BAND 0FFh] BAND 000ff0000h) BXOR
                   (Td4[(t3 SHR  8) BAND 0FFh] BAND 00000ff00h) BXOR
                   (Td4[(t2       ) BAND 0FFh] BAND 0000000ffh) BXOR
                   rk^[1];

        data[2] := (Td4[(t2 SHR 24)          ] BAND 0ff000000h) BXOR
                   (Td4[(t1 SHR 16) BAND 0FFh] BAND 000ff0000h) BXOR
                   (Td4[(t0 SHR  8) BAND 0FFh] BAND 00000ff00h) BXOR
                   (Td4[(t3       ) BAND 0FFh] BAND 0000000ffh) BXOR
                   rk^[2];

        data[3] := (Td4[(t3 SHR 24)          ] BAND 0ff000000h) BXOR
                   (Td4[(t2 SHR 16) BAND 0FFh] BAND 000ff0000h) BXOR
                   (Td4[(t1 SHR  8) BAND 0FFh] BAND 00000ff00h) BXOR
                   (Td4[(t0       ) BAND 0FFh] BAND 0000000ffh) BXOR
                   rk^[3];
    %ELSE
        data[0] := (ORD(Td4[(t0 SHR 24)          ]) SHL 24) BXOR
                   (ORD(Td4[(t3 SHR 16) BAND 0FFh]) SHL 16) BXOR
                   (ORD(Td4[(t2 SHR  8) BAND 0FFh]) SHL  8) BXOR
                   (ORD(Td4[(t1       ) BAND 0FFh])       ) BXOR
                   rk^[0];

        data[1] := (ORD(Td4[(t1 SHR 24)          ]) SHL 24) BXOR
                   (ORD(Td4[(t0 SHR 16) BAND 0FFh]) SHL 16) BXOR
                   (ORD(Td4[(t3 SHR  8) BAND 0FFh]) SHL  8) BXOR
                   (ORD(Td4[(t2       ) BAND 0FFh])       ) BXOR
                   rk^[1];

        data[2] := (ORD(Td4[(t2 SHR 24)          ]) SHL 24) BXOR
                   (ORD(Td4[(t1 SHR 16) BAND 0FFh]) SHL 16) BXOR
                   (ORD(Td4[(t0 SHR  8) BAND 0FFh]) SHL  8) BXOR
                   (ORD(Td4[(t3       ) BAND 0FFh])       ) BXOR
                   rk^[2];

        data[3] := (ORD(Td4[(t3 SHR 24)          ]) SHL 24) BXOR
                   (ORD(Td4[(t2 SHR 16) BAND 0FFh]) SHL 16) BXOR
                   (ORD(Td4[(t1 SHR  8) BAND 0FFh]) SHL  8) BXOR
                   (ORD(Td4[(t0       ) BAND 0FFh])       ) BXOR
                   rk^[3];
    %END
END decrypt;

PROCEDURE load(input : ADDRESS; VAR OUT d0, d1, d2, d3 : CARDINAL32) [INLINE];
VAR
    data        : POINTER TO DataBlock;
BEGIN
    (* alignment and endian dependent but faster.
       4 reads as opposed to 16.
    *)
    data := input;
    d0 := BIGENDIAN(data^[0]);
    d1 := BIGENDIAN(data^[1]);
    d2 := BIGENDIAN(data^[2]);
    d3 := BIGENDIAN(data^[3]);
END load;

PROCEDURE store(d0, d1, d2, d3 : CARDINAL32; output : ADDRESS) [INLINE];
VAR
    data        : POINTER TO DataBlock;
BEGIN
    (* alignment and endian dependent but faster.
       4 writes as opposed to 16.
    *)
    data := output;
    data^[0] := BIGENDIAN(d0);
    data^[1] := BIGENDIAN(d1);
    data^[2] := BIGENDIAN(d2);
    data^[3] := BIGENDIAN(d3);
END store;

PROCEDURE EncryptECB(crypt : AES; input, output : ADDRESS; amount : CARDINAL);
VAR
    <*/PUSH/NOCHECK:U*>
    data        : DataBlock;
    <*/POP*>
BEGIN
    IF (amount REM BlockSize) = 0 THEN
        REPEAT
            amount := amount - BlockSize;

            load(input, data[0], data[1], data[2], data[3]);
            encrypt(crypt, data);
            store(data[0], data[1], data[2], data[3], output);

            input := ADDADR(input, BlockSize);
            output := ADDADR(output, BlockSize);
        UNTIL amount = 0;
    END;
END EncryptECB;

PROCEDURE DecryptECB(crypt : AES; input, output : ADDRESS; amount : CARDINAL);
VAR
    <*/PUSH/NOCHECK:U*>
    data        : DataBlock;
    <*/POP*>
BEGIN
    IF (amount REM BlockSize) = 0 THEN
        REPEAT
            amount := amount - BlockSize;

            load(input, data[0], data[1], data[2], data[3]);
            decrypt(crypt, data);
            store(data[0], data[1], data[2], data[3], output);

            input := ADDADR(input, BlockSize);
            output := ADDADR(output, BlockSize);
        UNTIL amount = 0;
    END;
END DecryptECB;

PROCEDURE EncryptCBC(crypt : AES; input, output : ADDRESS; amount : CARDINAL);
VAR
    <*/PUSH/NOCHECK:U*>
    data        : DataBlock;
    <*/POP*>
    iv0, iv1,
    iv2, iv3    : CARDINAL32;
BEGIN
    IF (amount REM BlockSize) = 0 THEN
        load(ADR(crypt^.iv.bytes), iv0, iv1, iv2, iv3);

        REPEAT
            amount := amount - BlockSize;

            load(input, data[0], data[1], data[2], data[3]);
            data[0] := data[0] BXOR iv0;
            data[1] := data[1] BXOR iv1;
            data[2] := data[2] BXOR iv2;
            data[3] := data[3] BXOR iv3;
            encrypt(crypt, data);
            store(data[0], data[1], data[2], data[3], output);

            input := ADDADR(input, BlockSize);
            output := ADDADR(output, BlockSize);

            iv0 := data[0];
            iv1 := data[1];
            iv2 := data[2];
            iv3 := data[3];
        UNTIL amount = 0;

       store(iv0, iv1, iv2, iv3, ADR(crypt^.iv.bytes));
    END;
END EncryptCBC;

PROCEDURE DecryptCBC(crypt : AES; input, output : ADDRESS; amount : CARDINAL);
VAR
    <*/PUSH/NOCHECK:U*>
    data        : DataBlock;
    <*/POP*>
    s0, s1,
    s2, s3      : CARDINAL32;
    iv0, iv1,
    iv2, iv3    : CARDINAL32;
BEGIN
    IF (amount REM BlockSize) = 0 THEN
        load(ADR(crypt^.iv.bytes), iv0, iv1, iv2, iv3);

        REPEAT
            amount := amount - BlockSize;

            load(input, data[0], data[1], data[2], data[3]);
            s0 := data[0];
            s1 := data[1];
            s2 := data[2];
            s3 := data[3];
            decrypt(crypt, data);
            store(data[0] BXOR iv0,
                  data[1] BXOR iv1,
                  data[2] BXOR iv2,
                  data[3] BXOR iv3,
                  output);

            input := ADDADR(input, BlockSize);
            output := ADDADR(output, BlockSize);

            iv0 := s0;
            iv1 := s1;
            iv2 := s2;
            iv3 := s3;
        UNTIL amount = 0;

       store(iv0, iv1, iv2, iv3, ADR(crypt^.iv.bytes));
    END;
END DecryptCBC;

PROCEDURE EncryptCFB(crypt : AES; input, output : ADDRESS; amount : CARDINAL);
VAR
    i           : ADRCARD;
    num         : ADRCARD;
    <*/PUSH/NOCHECK:U*>
    data        : DataBlock;
    result      : ARRAY [0..BlockSize-1] OF BYTE;
    <*/POP*>
    in0, in1,
    in2, in3    : CARDINAL32;
    inP, outP   : POINTER TO ARRAY [0..BlockSize-1] OF BYTE;
BEGIN
    load(ADR(crypt^.iv.bytes), data[0], data[1], data[2], data[3]);

    REPEAT
        encrypt(crypt, data);

        IF amount >= BlockSize THEN
            (* full block *)
            amount := amount - BlockSize;

            load(input, in0, in1, in2, in3);
            data[0] := data[0] BXOR in0;
            data[1] := data[1] BXOR in1;
            data[2] := data[2] BXOR in2;
            data[3] := data[3] BXOR in3;
            store(data[0], data[1], data[2], data[3], output);

            input := ADDADR(input, BlockSize);
            output := ADDADR(output, BlockSize);
        ELSE
            (* partial block *)
            store(data[0], data[1], data[2], data[3], ADR(result));
            inP := input;
            outP := output;
            num := amount;
            FOR i := 0 TO num-1 DO
                outP^[i] := inP^[i] BXOR result[i];
            END;
            amount := 0;
        END;
    UNTIL amount = 0;

    store(data[0], data[1], data[2], data[3], ADR(crypt^.iv.bytes));
END EncryptCFB;

PROCEDURE DecryptCFB(crypt : AES; input, output : ADDRESS; amount : CARDINAL);
VAR
    i           : ADRCARD;
    num         : ADRCARD;
    <*/PUSH/NOCHECK:U*>
    data        : DataBlock;
    result      : ARRAY [0..BlockSize-1] OF BYTE;
    <*/POP*>
    in0, in1,
    in2, in3    : CARDINAL32;
    inP, outP   : POINTER TO ARRAY [0..BlockSize-1] OF BYTE;
BEGIN
    load(ADR(crypt^.iv.bytes), data[0], data[1], data[2], data[3]);

    REPEAT
        encrypt(crypt, data);

        IF amount >= BlockSize THEN
            (* full block *)
            amount := amount - BlockSize;

            load(input, in0, in1, in2, in3);
            store(data[0] BXOR in0,
                  data[1] BXOR in1,
                  data[2] BXOR in2,
                  data[3] BXOR in3,
                  output);

            data[0] := in0;
            data[1] := in1;
            data[2] := in2;
            data[3] := in3;

            input := ADDADR(input, BlockSize);
            output := ADDADR(output, BlockSize);
        ELSE
            (* partial block *)
            store(data[0], data[1], data[2], data[3], ADR(result));
            inP := input;
            outP := output;
            num := amount;
            FOR i := 0 TO num-1 DO
                outP^[i] := inP^[i] BXOR result[i];
            END;
            amount := 0;
        END;
    UNTIL amount = 0;

    store(data[0], data[1], data[2], data[3], ADR(crypt^.iv.bytes));
END DecryptCFB;

PROCEDURE SelfTest() : BOOLEAN;
TYPE
    array16     = ARRAY [0..15] OF BYTE;
    array24     = ARRAY [0..23] OF BYTE;
    array32     = ARRAY [0..31] OF BYTE;
    array64     = ARRAY [0..63] OF BYTE;

CONST
    (* offical test vectors from FIPS 197 document *)
    key128 = array16{00h, 01h, 02h, 03h, 04h, 05h, 06h, 07h,
                     08h, 09h, 0Ah, 0Bh, 0Ch, 0Dh, 0Eh, 0Fh};

    key192 = array24{00h, 01h, 02h, 03h, 04h, 05h, 06h, 07h,
                     08h, 09h, 0ah, 0bh, 0ch, 0dh, 0eh, 0fh,
                     10h, 11h, 12h, 13h, 14h, 15h, 16h, 17h};

    key256 = array32{00h, 01h, 02h, 03h, 04h, 05h, 06h, 07h,
                     08h, 09h, 0ah, 0bh, 0ch, 0dh, 0eh, 0fh,
                     10h, 11h, 12h, 13h, 14h, 15h, 16h, 17h,
                     18h, 19h, 1ah, 1bh, 1ch, 1dh, 1eh, 1fh};

    input = array16{000h, 011h, 022h, 033h, 044h, 055h, 066h, 077h,
                    088h, 099h, 0aah, 0bbh, 0cch, 0ddh, 0eeh, 0ffh};

    output128 = array16{069h, 0c4h, 0e0h, 0d8h, 06ah, 07bh, 004h, 030h,
                        0d8h, 0cdh, 0b7h, 080h, 070h, 0b4h, 0c5h, 05ah};

    output192 = array16{0ddh, 0a9h, 07ch, 0a4h, 086h, 04ch, 0dfh, 0e0h,
                        06eh, 0afh, 070h, 0a0h, 0ech, 00dh, 071h, 091h};

    output256 = array16{08eh, 0a2h, 0b7h, 0cah, 051h, 067h, 045h, 0bfh,
                        0eah, 0fch, 049h, 090h, 04bh, 049h, 060h, 089h};

    chainIV     = array16{55h BY 16};
    chainInput  = array64{77h BY 64};

TYPE
    tAlign = (* force 4-byte alignment of our test buffers *)
    RECORD
    dword       : CARDINAL32;
    input,
    result      : array16;
    input1,
    result1     : array64;
    END;
VAR
    crypt       : AES;
    data        : tAlign;
    iv          : IV;
    ok          : BOOLEAN;

    PROCEDURE verify(a, b : ARRAY OF BYTE) : BOOLEAN;
    VAR
        i       : CARDINAL;
    BEGIN
        FOR i := 0 TO HIGH(a) DO
            IF a[i] <> b[i] THEN
                RETURN FALSE;
            END;
        END;
        RETURN TRUE;
    END verify;

BEGIN
    ok := TRUE;

    crypt := Create(key128, 128);

    KeySetup(crypt, key128, 128);
    data.input := input;
    EncryptECB(crypt, ADR(data.input), ADR(data.result), 16);
    ok := ok AND verify(output128, data.result);
    DecryptECB(crypt, ADR(data.result), ADR(data.result), 16);
    ok := ok AND verify(input, data.result);

    KeySetup(crypt, key192, 192);
    data.input := input;
    EncryptECB(crypt, ADR(data.input), ADR(data.result), 16);
    ok := ok AND verify(output192, data.result);
    DecryptECB(crypt, ADR(data.result), ADR(data.result), 16);
    ok := ok AND verify(input, data.result);

    KeySetup(crypt, key256, 256);
    data.input := input;
    EncryptECB(crypt, ADR(data.input), ADR(data.result), 16);
    ok := ok AND verify(output256, data.result);
    DecryptECB(crypt, ADR(data.result), ADR(data.result), 16);
    ok := ok AND verify(input, data.result);

    (* now test the chaining modes *)

    (* no official test vectors,
       just make sure the input=>encrypt=>decrypt=>input sequence works.
    *)

    KeySetup(crypt, key128, 128);
    iv.bytes[0..15] := chainIV;
    data.input1 := chainInput;

    (* test CBC *)

    ResetIV(crypt, iv);
    EncryptCBC(crypt, ADR(data.input1), ADR(data.result1), 32);
    EncryptCBC(crypt, ADR(data.input1[32]), ADR(data.result1[32]), 32);
    ResetIV(crypt, iv);
    DecryptCBC(crypt, ADR(data.result1), ADR(data.result1), 32);
    DecryptCBC(crypt, ADR(data.result1[32]), ADR(data.result1[32]), 32);
    ok := ok AND verify(data.result1, data.input1);

    (* test CFB *)

    ResetIV(crypt, iv);
    EncryptCFB(crypt, ADR(data.input1), ADR(data.result1), 32);
    EncryptCFB(crypt, ADR(data.input1[32]), ADR(data.result1[32]), 13);
    ResetIV(crypt, iv);
    DecryptCFB(crypt, ADR(data.result1), ADR(data.result1), 32);
    DecryptCFB(crypt, ADR(data.result1[32]), ADR(data.result1[32]), 13);
    ok := ok AND verify(data.result1[0..44], data.input1);

    Destroy(crypt);

    RETURN ok;
END SelfTest;

END AES.
