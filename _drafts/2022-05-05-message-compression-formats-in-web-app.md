---
layout: post
title: 'Message compression formats in web app'
---

Compare the compression rates & encoding / decoding performance of small & large messages using these formats:

* [BSON](https://www.mongodb.com/json-and-bson)
* [CBOR](https://cbor.io/)
* [MessagePack](https://msgpack.org/)
* [Protobuf](https://developers.google.com/protocol-buffers/)
* [Thrift](https://github.com/apache/thrift)
* [Avro](https://github.com/apache/avro)
* [Cap'n'Proto](https://capnproto.org/)
* [FlatBuffers](https://github.com/google/flatbuffers)

Describe how we pass a protobuf-serialized object with a list of category IDs (and the difference between string IDs vs integer IDs) in the URL query parameters

Few data to test with:

* a sample object `Pet { name: string, kind: enum Kind { CAT, DOG } }`
* an object with an array of string identifiers `StringIdsResource { ids: string[] }`
* an object with an array of integer identifiers `StringIdsResource { ids: int[] }`

Things to compare:

* encoding & decoding performance in NodeJS and browser environment
* encoded message length (raw, as UTF-8 string and base-64 encoded UTF-8 string)
* amount of runtime (bundled code) needed in browser

Few lessons learned:

* Thrift library is super outdated, failed immediately trying to convert message to buffer with `TypeError: rw.byteLength is not a function`
* Protobuf and Avro provide by far the most compact output (because of schema provided)
* Protobuf library (protobufjs), unlike Avro, can't handle enumerations (Protobufjs requires raw integer values to be used whereas Avro supports semantic, string values)

## Raw results

### Test 1

```
CBOR-encode: 0.779ms
CBOR-decode: 0.587ms
[CBOR]> pre-utf8 (25): <Buffer b9 00 02 64 6e 61 6d 65 67 52 6f 64 72 69 67 6f 64 6b 69 6e 64 63 44 4f 47, dataView: DataView { byteLength: 25, byteOffset: 0, buffer: ArrayBuffer { [Uint8Contents]: <b9 00 02 64 6e 61 6d 65 67 52 6f 64 72 69 67 6f 64 6b 69 6e 64 63 44 4f 47 00 06 00 05 00 04 00 0f 05 00 0f 10 04 00 04 00 25 c6 58 36 01 00 00 01 00 00 00 10 3e 58 0f 01 ff ff ff 31 00 00 00 c8 2e c6 58 36 01 00 00 09 00 00 00 00 ce 65 1c 01 3c c6 58 36 01 00 00 00 00 00 00 00 00 00 00 b0 80 cc 58 ... 8092 more bytes>, byteLength: 8192 } }>
[CBOR]> post-utf8 (23): 适dnamegRodrigodkindcDOG
[CBOR]> base-64 (36): uQACZG5hbWVnUm9kcmlnb2RraW5kY0RPRw==
[CBOR]> decoded: { name: 'Rodrigo', kind: 'DOG' }
BSON-encode: 0.631ms
BSON-decode: 1.079ms
[BSON]> pre-utf8 (37): <Buffer 25 00 00 00 02 6e 61 6d 65 00 08 00 00 00 52 6f 64 72 69 67 6f 00 02 6b 69 6e 64 00 04 00 00 00 44 4f 47 00 00>
[BSON]> post-utf8 (37): %☻namRodrigo☻kind♦DOG
[BSON]> base-64 (52): JQAAAAJuYW1lAAgAAABSb2RyaWdvAAJraW5kAAQAAABET0cAAA==
[BSON]> decoded: { name: 'Rodrigo', kind: 'DOG' }
MessagePack-encode: 0.738ms
MessagePack-decode: 0.664ms
[MessagePack]> pre-utf8 (25): <Buffer de 00 02 a4 6e 61 6d 65 a7 52 6f 64 72 69 67 6f a4 6b 69 6e 64 a3 44 4f 47, dataView: DataView { byteLength: 25, byteOffset: 0, buffer: ArrayBuffer { [Uint8Contents]: <de 00 02 a4 6e 61 6d 65 a7 52 6f 64 72 69 67 6f a4 6b 69 6e 64 a3 44 4f 47 00 06 00 05 00 04 00 0f 05 00 0f 10 04 00 04 00 25 c6 58 36 01 00 00 01 00 00 00 10 3e 58 0f 01 ff ff ff 31 00 00 00 28 2e c6 58 36 01 00 00 08 00 00 00 00 ce 65 1c 01 fc c5 58 36 01 00 00 b0 2e c6 58 36 01 00 00 0c 00 00 00 ... 8092 more bytes>, byteLength: 8192 } }>
[MessagePack]> post-utf8 (16): ހ☻䮡me璯drigo䫩ndㄏG
[MessagePack]> base-64 (36): 3gACpG5hbWWnUm9kcmlnb6RraW5ko0RPRw==
[MessagePack]> decoded: { name: 'Rodrigo', kind: 'DOG' }
Avro-encode: 0.199ms
Avro-decode: 0.181ms
[Avro]> pre-utf8 (9): <Buffer 02 0e 52 6f 64 72 69 67 6f>
[Avro]> post-utf8 (9): ☻♫Rodrigo
[Avro]> base-64 (12): Ag5Sb2RyaWdv
[Avro]> decoded: Pet { kind: 'DOG', name: 'Rodrigo' }
Protobuf-encode: 2.723ms
Protobuf-decode: 0.231ms
[Protobuf]> pre-utf8 (11): <Buffer 08 00 12 07 52 6f 64 72 69 67 6f>
[Protobuf]> post-utf8 (11):↕Rodrigo
[Protobuf]> base-64 (16): CAASB1JvZHJpZ28=
[Protobuf]> decoded: Pet { kind: 0, name: 'Rodrigo' }
```

### Test 2

```
CBOR-encode: 0.851ms
CBOR-decode: 0.614ms
[CBOR]> pre-utf8 (63): <Buffer b9 00 01 63 69 64 73 84 6c 70 61 72 74 6e 65 72 31 2d 30 30 31 6c 70 61 72 74 6e 65 72 32 2d 30 30 31 6f 70 61 72 74 6e 65 72 31 2d 31 30 34 32 30 31 ... 13 more bytes, dataView: 
DataView { byteLength: 63, byteOffset: 0, buffer: ArrayBuffer { [Uint8Contents]: <b9 00 01 63 69 64 73 84 6c 70 61 72 74 6e 65 72 31 2d 30 30 31 6c 70 61 72 74 6e 65 72 32 2d 30 30 31 6f 70 61 72 74 6e 65 72 31 2d 31 30 34 32 30 31 6c 70 61 72 74 6e 65 72 32 2d 30 30 33 00 e8 42 62 db bd 01 00 00 00 00 00 00 c1 2d 6c 01 01 99 75 db bd 01 00 00 a8 ee 75 db bd 01 00 00 0c 00 00 00 ... 8092 more bytes>, byteLength: 8192 } }>
[CBOR]> post-utf8 (59): 送cids䬰artner1-001lpartner2-001opartner1-104201lpartner2-003
[CBOR]> base-64 (84): uQABY2lkc4RscGFydG5lcjEtMDAxbHBhcnRuZXIyLTAwMW9wYXJ0bmVyMS0xMDQyMDFscGFydG5lcjItMDAz
[CBOR]> decoded: {
  ids: [ 'partner1-001', 'partner2-001', 'partner1-104201', 'partner2-003' ]
}
BSON-encode: 0.69ms
BSON-decode: 0.922ms
[BSON]> pre-utf8 (98): <Buffer 62 00 00 00 04 69 64 73 00 58 00 00 00 02 30 00 0d 00 00 00 70 61 72 74 6e 65 72 31 2d 30 30 31 00 02 31 00 0d 00 00 00 70 61 72 74 6e 65 72 32 2d 30 ... 48 more bytes>
partner2-003☻2►partner1-104201☻3
[BSON]> base-64 (132): YgAAAARpZHMAWAAAAAIwAA0AAABwYXJ0bmVyMS0wMDEAAjEADQAAAHBhcnRuZXIyLTAwMQACMgAQAAAAcGFydG5lcjEtMTA0MjAxAAIzAA0AAABwYXJ0bmVyMi0wMDMAAAA=
[BSON]> decoded: {
  ids: [ 'partner1-001', 'partner2-001', 'partner1-104201', 'partner2-003' ]
}
MessagePack-encode: 0.713ms
MessagePack-decode: 0.649ms
[MessagePack]> pre-utf8 (63): <Buffer de 00 01 a3 69 64 73 94 ac 70 61 72 74 6e 65 72 31 2d 30 30 31 ac 70 61 72 74 6e 65 72 32 2d 30 30 31 af 70 61 72 74 6e 65 72 31 2d 31 30 34 32 30 31 ... 13 more bytes, dataView: DataView { byteLength: 63, byteOffset: 0, buffer: ArrayBuffer { [Uint8Contents]: <de 00 01 a3 69 64 73 94 ac 70 61 72 74 6e 65 72 31 2d 30 30 31 ac 70 61 72 74 6e 65 72 32 2d 30 30 31 af 70 61 72 74 6e 65 72 31 2d 31 30 34 32 30 31 ac 70 61 72 74 6e 65 72 32 2d 30 30 33 00 e8 42 62 db bd 01 00 00 00 00 00 00 c1 2d 6c 01 01 ba 75 db bd 01 00 00 30 ec 75 db bd 01 00 00 09 00 00 00 ... 8092 more bytes>, byteLength: 8192 } }>
[MessagePack]> post-utf8 (52): ހ☺㩤s䬰artner1-001찡rtner2-001ﰡrtner1-104201찡rtner2-003
[MessagePack]> base-64 (84): 3gABo2lkc5SscGFydG5lcjEtMDAxrHBhcnRuZXIyLTAwMa9wYXJ0bmVyMS0xMDQyMDGscGFydG5lcjItMDAz
[MessagePack]> decoded: {
  ids: [ 'partner1-001', 'partner2-001', 'partner1-104201', 'partner2-003' ]
}
Avro-encode: 0.22ms
Avro-decode: 0.205ms
[Avro]> pre-utf8 (57): <Buffer 08 18 70 61 72 74 6e 65 72 31 2d 30 30 31 18 70 61 72 74 6e 65 72 32 2d 30 30 31 1e 70 61 72 74 6e 65 72 31 2d 31 30 34 32 30 31 18 70 61 72 74 6e 65 ... 7 more bytes>
[Avro]> post-utf8 (57):↑partner1-001↑partner2-001▲partner1-104201↑partner2-003
[Avro]> base-64 (76): CBhwYXJ0bmVyMS0wMDEYcGFydG5lcjItMDAxHnBhcnRuZXIxLTEwNDIwMRhwYXJ0bmVyMi0wMDMA
[Avro]> decoded: StringIdsResource {
  ids: [ 'partner1-001', 'partner2-001', 'partner1-104201', 'partner2-003' ]
}
Protobuf-encode: 3.12ms
Protobuf-decode: 0.261ms
[Protobuf]> pre-utf8 (59): <Buffer 0a 0c 70 61 72 74 6e 65 72 31 2d 30 30 31 0a 0c 70 61 72 74 6e 65 72 32 2d 30 30 31 0a 0f 70 61 72 74 6e 65 72 31 2d 31 30 34 32 30 31 0a 0c 70 61 72 ... 9 more bytes>
[Protobuf]> post-utf8 (59): 
♀partner1-001
♀partner2-001
☼partner1-104201
♀partner2-003
[Protobuf]> base-64 (80): CgxwYXJ0bmVyMS0wMDEKDHBhcnRuZXIyLTAwMQoPcGFydG5lcjEtMTA0MjAxCgxwYXJ0bmVyMi0wMDM=
[Protobuf]> decoded: StringIdsResource {
  ids: [ 'partner1-001', 'partner2-001', 'partner1-104201', 'partner2-003' ]
}
```

### Test 3

```
CBOR-encode: 0.777ms
CBOR-decode: 0.583ms
[CBOR]> pre-utf8 (26): <Buffer b9 00 01 63 69 64 73 84 1a 00 0f 9c 19 1a 00 0f a2 6d 19 80 07 1a 00 01 31 1d, dataView: DataView { byteLength: 26, byteOffset: 0, buffer: ArrayBuffer { [Uint8Contents]: <b9 00 01 63 69 64 73 84 1a 00 0f 9c 19 1a 00 0f a2 6d 19 80 07 1a 00 01 31 1d 04 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 ff ff ff ff 0a 00 00 00 00 ff ff ff 31 00 00 00 08 c4 d5 19 d3 01 00 00 00 00 00 00 21 55 a7 22 00 e2 d5 19 d3 01 00 00 00 00 00 00 00 00 00 00 80 d6 c4 0f ... 8092 more bytes>, byteLength: 8192 } }>
[CBOR]> post-utf8 (16): 送cids䚀☼왚☼⭙ǚ☺1↔
[CBOR]> base-64 (36): uQABY2lkc4QaAA+cGRoAD6JtGYAHGgABMR0=
[CBOR]> decoded: { ids: [ 1023001, 1024621, 32775, 78109 ] }
BSON-encode: 0.857ms
BSON-decode: 0.823ms
[BSON]> pre-utf8 (43): <Buffer 2b 00 00 00 04 69 64 73 00 21 00 00 00 10 30 00 19 9c 0f 00 10 31 00 6d a2 0f 00 10 32 00 07 80 00 00 10 33 00 1d 31 01 00 00 00>
[BSON]> post-utf8 (37): +♦ids!►0↓쏀►1m⏀►2►3↔1☺
[BSON]> base-64 (60): KwAAAARpZHMAIQAAABAwABmcDwAQMQBtog8AEDIAB4AAABAzAB0xAQAAAA==
[BSON]> decoded: { ids: [ 1023001, 1024621, 32775, 78109 ] }
MessagePack-encode: 0.655ms
MessagePack-decode: 0.619ms
[MessagePack]> pre-utf8 (26): <Buffer de 00 01 a3 69 64 73 94 ce 00 0f 9c 19 ce 00 0f a2 6d cd 80 07 ce 00 01 31 1d, dataView: DataView { byteLength: 26, byteOffset: 0, buffer: ArrayBuffer { [Uint8Contents]: <de 00 01 a3 69 64 73 94 ce 00 0f 9c 19 ce 00 0f a2 6d cd 80 07 ce 00 01 31 1d 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 ... 8092 more bytes>, byteLength: 8192 } }>
[MessagePack]> post-utf8 (15): ހ☺㩤s䎀☼왎☼⭍ǎ☺1↔
[MessagePack]> base-64 (36): 3gABo2lkc5TOAA+cGc4AD6JtzYAHzgABMR0=
[MessagePack]> decoded: { ids: [ 1023001, 1024621, 32775, 78109 ] }
Avro-encode: 0.105ms
Avro-decode: 0.169ms
[Avro]> pre-utf8 (14): <Buffer 08 b2 f0 7c da 89 7d 8e 80 04 ba c4 09 00>
[Avro]> post-utf8 (7):ⰼډ}ꄉ
[Avro]> base-64 (20): CLLwfNqJfY6ABLrECQA=
[Avro]> decoded: IntIdsResource { ids: [ 1023001, 1024621, 32775, 78109 ] }
Protobuf-encode: 2.624ms
Protobuf-decode: 0.219ms
[Protobuf]> pre-utf8 (14): <Buffer 0a 0c 99 b8 3e ed c4 3e 87 80 02 9d e2 04>
[Protobuf]> post-utf8 (6):
♀鸾턾瀂�
[Protobuf]> base-64 (20): CgyZuD7txD6HgAKd4gQ=
[Protobuf]> decoded: IntIdsResource { ids: [ 1023001, 1024621, 32775, 78109 ] }
```

### Test 4

```
CBOR-encode: 1.589ms
CBOR-decode: 0.814ms
[CBOR]> pre-utf8 (905): <Buffer b9 00 01 6a 64 61 74 61 50 6f 69 6e 74 73 8a b9 00 02 69 74 69 6d 65 73 74 61 6d 70 fb 42 78 09 3e ad 39 00 00 66 76 61 6c 75 65 73 b9 00 03 69 63 61 ... 855 more bytes, dataView: DataView { byteLength: 905, byteOffset: 0, buffer: ArrayBuffer { [Uint8Contents]: <b9 00 01 6a 64 61 74 61 50 6f 69 6e 74 73 8a b9 00 02 69 74 69 6d 65 73 74 61 6d 70 fb 42 78 09 3e ad 39 00 00 66 76 61 6c 75 65 73 b9 00 03 69 63 61 74 65 67 6f 72 79 31 fb c0 14 d4 17 04 b5 8c f9 69 63 61 74 65 67 6f 72 79 32 fb 40 56 52 ff a3 40 fa cc 69 63 61 74 65 67 6f 72 79 33 fb 40 41 f4 43 ... 8092 more bytes>, byteLength: 8192 } }>
[CBOR]> post-utf8 (701): 送jdataPoints김☻itimestamp󂸉>� fvalues逃icategory1󀔔 ↨♦匹icategory2󀖒��̩category3󀁴C!X-김  ☻itimestamp󂸉>� fvalues逃icategory1󀅭ѕEȆicategory2󀵘ǳr  ↕ةcategory3󀟥#��适itimestamp󂸉>�  fvalues逃icat
egory1󀫆I裁iicategory2󀋳7_㶁icategory3󀻐�뒹   ☻itimestamp󂸉>� fvalues逃icategory1󀻉 ◄Q^L񩣡tegory2󀰇%Ż?䩣ategory3󀡲~縰ڹ   ☻itimestamp󂸉>� fvalues逃icategory1󀅔锉category2󀓪s顖zicategory3󀞮꜅gF适itimestamp󂸉>�    fvalues逃icategory1󀱜ox݋icategory2󀕛��>icategory3󀕞,^  ∟爹☻itimestamp󂸉>� fvalues逃icategory1󀐋ݸ9ﭩcategory2󀬋'沕ꩣategory3󀥟ɘEł适itimestamp󂸉>�    fvalues逃icategory1󀏂훣΂icategory2󀂳4%E@Sicategory3󀡛  F适itimestamp󂸉>� fvalues逃icategory1󀒚 ⌂樤Aicategory2󀅌뜚歩category3󀦆억  ↑7适itimestamp󂸉>� fvalues逃icategory1󀴐xI槑icategory2󀞷y$񊅩category3󀤏��
[CBOR]> base-64 (1208): uQABamRhdGFQb2ludHOKuQACaXRpbWVzdGFtcPtCeAk+rTkAAGZ2YWx1ZXO5AANpY2F0ZWdvcnkx+8AU1BcEtYz5aWNhdGVnb3J5MvtAVlL/o0D6zGljYXRlZ29yeTP7QEH0QyFYLYq5AAJpdGltZXN0YW1w+0J4CT6tOQAAZnZhbHVlc7kAA2ljYXRlZ29yeTH7wEWt0dVFyIZpY2F0ZWdvcnky+0A12MczchLYaWNhdGVnb3J5M/tAXyUj/u3YFrkAAml0aW1lc3RhbXD7QngJPq05AABmdmFsdWVzuQADaWNhdGVnb3J5MftAK4ZJqKOBaWljYXRlZ29yeTL7wEtzN1+jtsFpY2F0ZWdvcnkz+0A7kL2/lqvSuQACaXRpbWVzdGFtcPtCeAk+rTkAAGZ2YWx1ZXO5AANpY2F0ZWdvcnkx+8A7CRFRXkz5aWNhdGVnb3J5MvvAMMclxXs/hGljYXRlZ29yeTP7QGGyfpe48Nq5AAJpdGltZXN0YW1w+0J4CT6tOQAAZnZhbHVlc7kAA2ljYXRlZ29yeTH7wAXUuVRJvuBpY2F0ZWdvcnky+8BTanPpoRZ6aWNhdGVnb3J5M/tAXi6K3MVnRrkAAml0aW1lc3RhbXD7QngJPq05AABmdmFsdWVzuQADaWNhdGVnb3J5MfvAMVwIb3jdy2ljYXRlZ29yeTL7QFVb/Lm4Lz5pY2F0ZWdvcnkz+0BV3ixeHKdIuQACaXRpbWVzdGFtcPtCeAk+rTkAAGZ2YWx1ZXO5AANpY2F0ZWdvcnkx+0BQy904Ob9taWNhdGVnb3J5MvtALIsntnIVumljYXRlZ29yeTP7QCXfyRhFxYK5AAJpdGltZXN0YW1w+0J4CT6tOQAAZnZhbHVlc7kAA2ljYXRlZ29yeTH7QE9CvdtjzsJpY2F0ZWdvcnky+8BC8zQlRUBTaWNhdGVnb3J5M/tAYRsInhCwRrkAAml0aW1lc3RhbXD7QngJPq05AABmdmFsdWVzuQADaWNhdGVnb3J5MftAUhp/tijkQWljYXRlZ29yeTL7QEUMu5yahm1pY2F0ZWdvcnkz+0BmBowW9Rg3uQACaXRpbWVzdGFtcPtCeAk+rTkAAGZ2YWx1ZXO5AANpY2F0ZWdvcnkx+0A0UHhJlifRaWNhdGVnb3J5MvtAXnd5JPEKRWljYXRlZ29yeTP7QGQP/dXhjO8=
[CBOR]> decoded: {
  dataPoints: [
    { timestamp: 1651749082000, values: [Object] },
    { timestamp: 1651749082000, values: [Object] },
    { timestamp: 1651749082000, values: [Object] },
    { timestamp: 1651749082000, values: [Object] },
    { timestamp: 1651749082000, values: [Object] },
    { timestamp: 1651749082000, values: [Object] },
    { timestamp: 1651749082000, values: [Object] },
    { timestamp: 1651749082000, values: [Object] },
    { timestamp: 1651749082000, values: [Object] },
    { timestamp: 1651749082000, values: [Object] }
  ]
}
BSON-encode: 3.156ms
BSON-decode: 1.624ms
[BSON]> pre-utf8 (992): <Buffer e0 03 00 00 04 64 61 74 61 50 6f 69 6e 74 73 00 cf 03 00 00 03 30 00 5e 00 00 00 01 74 69 6d 65 73 74 61 6d 70 00 00 00 39 ad 3e 09 78 42 03 76 61 6c ... 942 more bytes>
[BSON]> post-utf8 (872): À♦dataPointsσ♥0^☺timestamp9�xB♥values>☺category1񌵄 ↨Ԕ☺category2̺@㿒V@☺category3ꭘ!C􁀀 ♥1^☺timestamp9�xB♥values>☺category1戅Ց텀☺category2ؒr3ǘ5@☺category3▬ح��@♥2^☺timestamp9�xB♥values>☺categ
ory1iᣨI櫀☺category2v㟷sK☺category3ҫ濽ເ♥3^☺timestamp9�xB♥values>☺category1񌞑 ◄    ;☺category2俻ťǰ☺category3ڰ藾⡀♥4^☺timestamp9�xB♥values>☺category1ྉT锅☺category2z▬ᩳjS☺category3FgŜꮞ@♥5^☺timestamp9�xB♥values>☺catego
ry1˝x\1☺category2>/蹼[U@☺category3H眞,ޕ@♥6^☺timestamp9�xB♥values>☺category1mﹸ݋P@☺category2ꕲ構,@☺category3ⅅ↑ɟ%@♥7^☺timestamp9�xB♥values>☺category1c۽BO@☺category2S@E%4󂀁category3 F@♥8^☺timestamp9�xB♥values>☺categ
ory1A䨶⌂→R@☺category2m暜댅@☺category37↑��f@♥9^☺timestamp9�xB♥values>☺category1ѧ扸P4@☺category2E
񤹷^@ ☺category3ս☼d@
[BSON]> base-64 (1324): 4AMAAARkYXRhUG9pbnRzAM8DAAADMABeAAAAAXRpbWVzdGFtcAAAADmtPgl4QgN2YWx1ZXMAPgAAAAFjYXRlZ29yeTEA+Yy1BBfUFMABY2F0ZWdvcnkyAMz6QKP/UlZAAWNhdGVnb3J5MwCKLVghQ/RBQAAAAzEAXgAAAAF0aW1lc3RhbXAAAAA5rT4JeEIDdmFsdWVzAD4AAAABY2F0ZWdvcnkxAIbIRdXRrUXAAWNhdGVnb3J5MgDYEnIzx9g1QAFjYXRlZ29yeTMAFtjt/iMlX0AAAAMyAF4AAAABdGltZXN0YW1wAAAAOa0+CXhCA3ZhbHVlcwA+AAAAAWNhdGVnb3J5MQBpgaOoSYYrQAFjYXRlZ29yeTIAwbajXzdzS8ABY2F0ZWdvcnkzANKrlr+9kDtAAAADMwBeAAAAAXRpbWVzdGFtcAAAADmtPgl4QgN2YWx1ZXMAPgAAAAFjYXRlZ29yeTEA+UxeUREJO8ABY2F0ZWdvcnkyAIQ/e8UlxzDAAWNhdGVnb3J5MwDa8LiXfrJhQAAAAzQAXgAAAAF0aW1lc3RhbXAAAAA5rT4JeEIDdmFsdWVzAD4AAAABY2F0ZWdvcnkxAOC+SVS51AXAAWNhdGVnb3J5MgB6FqHpc2pTwAFjYXRlZ29yeTMARmfF3IouXkAAAAM1AF4AAAABdGltZXN0YW1wAAAAOa0+CXhCA3ZhbHVlcwA+AAAAAWNhdGVnb3J5MQDL3XhvCFwxwAFjYXRlZ29yeTIAPi+4ufxbVUABY2F0ZWdvcnkzAEinHF4s3lVAAAADNgBeAAAAAXRpbWVzdGFtcAAAADmtPgl4QgN2YWx1ZXMAPgAAAAFjYXRlZ29yeTEAbb85ON3LUEABY2F0ZWdvcnkyALoVcrYniyxAAWNhdGVnb3J5MwCCxUUYyd8lQAAAAzcAXgAAAAF0aW1lc3RhbXAAAAA5rT4JeEIDdmFsdWVzAD4AAAABY2F0ZWdvcnkxAMLOY9u9Qk9AAWNhdGVnb3J5MgBTQEUlNPNCwAFjYXRlZ29yeTMARrAQnggbYUAAAAM4AF4AAAABdGltZXN0YW1wAAAAOa0+CXhCA3ZhbHVlcwA+AAAAAWNhdGVnb3J5MQBB5Ci2fxpSQAFjYXRlZ29yeTIAbYaanLsMRUABY2F0ZWdvcnkzADcY9RaMBmZAAAADOQBeAAAAAXRpbWVzdGFtcAAAADmtPgl4QgN2YWx1ZXMAPgAAAAFjYXRlZ29yeTEA0SeWSXhQNEABY2F0ZWdvcnkyAEUK8SR5d15AAWNhdGVnb3J5MwDvjOHV/Q9kQAAAAAA=
[BSON]> decoded: {
  dataPoints: [
    { timestamp: 1651749082000, values: [Object] },
    { timestamp: 1651749082000, values: [Object] },
    { timestamp: 1651749082000, values: [Object] },
    { timestamp: 1651749082000, values: [Object] },
    { timestamp: 1651749082000, values: [Object] },
    { timestamp: 1651749082000, values: [Object] },
    { timestamp: 1651749082000, values: [Object] },
    { timestamp: 1651749082000, values: [Object] },
    { timestamp: 1651749082000, values: [Object] },
    { timestamp: 1651749082000, values: [Object] }
  ]
}
MessagePack-encode: 1.497ms
MessagePack-decode: 0.864ms
[MessagePack]> pre-utf8 (905): <Buffer de 00 01 aa 64 61 74 61 50 6f 69 6e 74 73 9a de 00 02 a9 74 69 6d 65 73 74 61 6d 70 cb 42 78 09 3e ad 39 00 00 a6 76 61 6c 75 65 73 de 00 03 a9 63 61 ... 855 more bytes, dataView: DataView { byteLength: 905, byteOffset: 0, buffer: ArrayBuffer { [Uint8Contents]: <de 00 01 aa 64 61 74 61 50 6f 69 6e 74 73 9a de 00 02 a9 74 69 6d 65 73 74 61 6d 70 cb 42 78 09 3e ad 39 00 00 a6 76 61 6c 75 65 73 de 00 03 a9 63 61 74 65 67 6f 72 79 31 cb c0 14 d4 17 04 b5 8c f9 a9 63 61 74 65 67 6f 72 79 32 cb 40 56 52 ff a3 40 fa cc a9 63 61 74 65 67 6f 72 79 33 cb 40 41 f4 43 ... 8092 more bytes>, byteLength: 8192 } }>
[MessagePack]> post-utf8 (661): ހ☺ꤡtaPointsꞀ☻鴩mestamp˂x        >�涡luesހ♥飡tegory1ˀ¶ԗ♦匹飡tegory2ˀVR��̩category3ˀA􃡘-Ꞁ ☻鴩mestamp˂x     >�涡luesހ♥飡tegory1ˀE푕EȆ飡tegory2ˀ5؇3r↕ةcategory3ˀ_%#��ހ☻鴩mestamp˂x   >�
涡luesހ♥飡tegory1ˀ+扨ど飡tegory2ˀKs7_㶁飡tegory3ˀ;ཿ櫒ހ☻鴩mestamp˂x      >�涡luesހ♥飡tegory1ˀ;   ◄Q^L񩣡tegory2ˀ0ǥŻ?䩣ategory3ˀa⾗谚ހ ☻鴩mestamp˂x >�涡luesހ♥飡tegory1ˀ♣ԹTIcategory2ˀSjs顖z飡tegory3ˀ^.꜅gFހ☻鴩mestamp˂x     >�涡luesހ♥飡tegory1ˀ1ox݋飡tegory2ˀU[��>飡tegory3ˀUެ^∟爞☻鴩mestamp˂x     >�涡luesހ♥飡tegory1ˀP˝89ﭩcategory2ˀ,맶r§ꩣategory3ˀ%߉↑Ełހ☻鴩mestamp˂x    >�涡luesހ♥飡tegory1ˀOB훣΂飡tegory2ˀB󴥅@S飡tegory3ˀaFހ☻鴩mes
tamp˂x  >�涡luesހ♥飡tegory1ˀR→⌂樤A飡tegory2ˀE♀뜚歩category3ˀf♠억↑7ހ☻鴩mestamp˂x >�涡luesހ♥飡tegory1ˀ4PxI槑飡tegory2ˀ^wy$񊅩category3ˀd ☼��
[MessagePack]> base-64 (1208): 3gABqmRhdGFQb2ludHOa3gACqXRpbWVzdGFtcMtCeAk+rTkAAKZ2YWx1ZXPeAAOpY2F0ZWdvcnkxy8AU1BcEtYz5qWNhdGVnb3J5MstAVlL/o0D6zKljYXRlZ29yeTPLQEH0QyFYLYreAAKpdGltZXN0YW1wy0J4CT6tOQAApnZhbHVlc94AA6ljYXRlZ29yeTHLwEWt0dVFyIapY2F0ZWdvcnkyy0A12MczchLYqWNhdGVnb3J5M8tAXyUj/u3YFt4AAql0aW1lc3RhbXDLQngJPq05AACmdmFsdWVz3gADqWNhdGVnb3J5MctAK4ZJqKOBaaljYXRlZ29yeTLLwEtzN1+jtsGpY2F0ZWdvcnkzy0A7kL2/lqvS3gACqXRpbWVzdGFtcMtCeAk+rTkAAKZ2YWx1ZXPeAAOpY2F0ZWdvcnkxy8A7CRFRXkz5qWNhdGVnb3J5MsvAMMclxXs/hKljYXRlZ29yeTPLQGGyfpe48NreAAKpdGltZXN0YW1wy0J4CT6tOQAApnZhbHVlc94AA6ljYXRlZ29yeTHLwAXUuVRJvuCpY2F0ZWdvcnkyy8BTanPpoRZ6qWNhdGVnb3J5M8tAXi6K3MVnRt4AAql0aW1lc3RhbXDLQngJPq05AACmdmFsdWVz3gADqWNhdGVnb3J5McvAMVwIb3jdy6ljYXRlZ29yeTLLQFVb/Lm4Lz6pY2F0ZWdvcnkzy0BV3ixeHKdI3gACqXRpbWVzdGFtcMtCeAk+rTkAAKZ2YWx1ZXPeAAOpY2F0ZWdvcnkxy0BQy904Ob9tqWNhdGVnb3J5MstALIsntnIVuqljYXRlZ29yeTPLQCXfyRhFxYLeAAKpdGltZXN0YW1wy0J4CT6tOQAApnZhbHVlc94AA6ljYXRlZ29yeTHLQE9CvdtjzsKpY2F0ZWdvcnkyy8BC8zQlRUBTqWNhdGVnb3J5M8tAYRsInhCwRt4AAql0aW1lc3RhbXDLQngJPq05AACmdmFsdWVz3gADqWNhdGVnb3J5MctAUhp/tijkQaljYXRlZ29yeTLLQEUMu5yahm2pY2F0ZWdvcnkzy0BmBowW9Rg33gACqXRpbWVzdGFtcMtCeAk+rTkAAKZ2YWx1ZXPeAAOpY2F0ZWdvcnkxy0A0UHhJlifRqWNhdGVnb3J5MstAXnd5JPEKRaljYXRlZ29yeTPLQGQP/dXhjO8=
[MessagePack]> decoded: {
  dataPoints: [
    { timestamp: 1651749082000, values: [Object] },
    { timestamp: 1651749082000, values: [Object] },
    { timestamp: 1651749082000, values: [Object] },
    { timestamp: 1651749082000, values: [Object] },
    { timestamp: 1651749082000, values: [Object] },
    { timestamp: 1651749082000, values: [Object] },
    { timestamp: 1651749082000, values: [Object] },
    { timestamp: 1651749082000, values: [Object] },
    { timestamp: 1651749082000, values: [Object] },
    { timestamp: 1651749082000, values: [Object] }
  ]
}
Avro-encode: 2.03ms
Avro-decode: 1.752ms
[Avro]> pre-utf8 (302): <Buffer 14 a0 ce d6 be 92 60 f9 8c b5 04 17 d4 14 c0 cc fa 40 a3 ff 52 56 40 8a 2d 58 21 43 f4 41 40 a0 ce d6 be 92 60 86 c8 45 d5 d1 ad 45 c0 d8 12 72 33 c7 ... 252 more bytes>
[Avro]> post-utf8 (171): ¶Ζ񌵄 ↨Ԕ♀򀣿RV@ꭘ!C􁀠Ζ戅Ց텀ؒr3ǘ5@  ▬ح��@ΖiᣨI櫀v㟷sK↕떿퐻@Ζ񌞑 ◄    ;♦?{ťǰ→𸗾⡀ΖྉT锅:▬ᩳjS♠gŜꮞ@Ζ˝x\1>/蹼[U@H眞,ޕ@Ζmﹸ݋P@ꕲ構,@ⅅ↑ɟ%@Ζc۽BO@S@E%4󂀆О@ΖA䨶⌂→R@m暜댅@7↑��f@Ζѧ扸P4@E
񤹷^@ս ☼d@
[Avro]> base-64 (404): FKDO1r6SYPmMtQQX1BTAzPpAo/9SVkCKLVghQ/RBQKDO1r6SYIbIRdXRrUXA2BJyM8fYNUAW2O3+IyVfQKDO1r6SYGmBo6hJhitAwbajXzdzS8DSq5a/vZA7QKDO1r6SYPlMXlERCTvAhD97xSXHMMDa8LiXfrJhQKDO1r6SYOC+SVS51AXAehah6XNqU8BGZ8Xcii5eQKDO1r6SYMvdeG8IXDHAPi+4ufxbVUBIpxxeLN5VQKDO1r6SYG2/OTjdy1BAuhVytieLLECCxUUYyd8lQKDO1r6SYMLOY9u9Qk9AU0BFJTTzQsBGsBCeCBthQKDO1r6SYEHkKLZ/GlJAbYaanLsMRUA3GPUWjAZmQKDO1r6SYNEnlkl4UDRARQrxJHl3XkDvjOHV/Q9kQAA=
[Avro]> decoded: TimeframeResource {
  dataPoints: [
    DataPoint { timestamp: 1651749082000n, values: [TimeframeValues] },
    DataPoint { timestamp: 1651749082000n, values: [TimeframeValues] },
    DataPoint { timestamp: 1651749082000n, values: [TimeframeValues] },
    DataPoint { timestamp: 1651749082000n, values: [TimeframeValues] },
    DataPoint { timestamp: 1651749082000n, values: [TimeframeValues] },
    DataPoint { timestamp: 1651749082000n, values: [TimeframeValues] },
    DataPoint { timestamp: 1651749082000n, values: [TimeframeValues] },
    DataPoint { timestamp: 1651749082000n, values: [TimeframeValues] },
    DataPoint { timestamp: 1651749082000n, values: [TimeframeValues] },
    DataPoint { timestamp: 1651749082000n, values: [TimeframeValues] }
  ]
}
Protobuf-encode: 6.154ms
Protobuf-decode: 7.376ms
[Protobuf]> pre-utf8 (380): <Buffer 0a 24 08 90 a7 ab 9f 89 30 12 1b 09 f9 8c b5 04 17 d4 14 c0 11 cc fa 40 a3 ff 52 56 40 19 8a 2d 58 21 43 f4 41 40 0a 24 08 90 a7 ab 9f 89 30 12 1b 09 ... 330 more bytes>     
[Protobuf]> post-utf8 (243):
৫↕     �↨Ԕ◄̺@㿒V@↓ꭘ!C􁀊$৫↕     Ց텀◄ؒr3ǘ5@↓▬ح��@
৫↕     ᣨI櫀◄v㟷sK↓ҫ濽ເ
৫↕     �◄      ;◄俻ťǰ↓ڰ藾⡀
৫↕     T锅◄z▬ᩳjS↓FgŜꮞ@
৫↕     x\1◄>/蹼[U@↓H眞,ޕ@
৫↕     ﹸ݋P@◄ꕲ構,@↓ⅅ↑ɟ%@
৫↕     䨶⌂→R@◄m暜댅@↓7↑��f@
৫↕     扸P4@◄E
񤹷^@ ↓ս☼d@
[Protobuf]> base-64 (508): CiQIkKern4kwEhsJ+Yy1BBfUFMARzPpAo/9SVkAZii1YIUP0QUAKJAiQp6ufiTASGwmGyEXV0a1FwBHYEnIzx9g1QBkW2O3+IyVfQAokCJCnq5+JMBIbCWmBo6hJhitAEcG2o183c0vAGdKrlr+9kDtACiQIkKern4kwEhsJ+UxeUREJO8ARhD97xSXHMMAZ2vC4l36yYUAKJAiQp6ufiTASGwngvklUudQFwBF6FqHpc2pTwBlGZ8Xcii5eQAokCJCnq5+JMBIbCcvdeG8IXDHAET4vuLn8W1VAGUinHF4s3lVACiQIkKern4kwEhsJbb85ON3LUEARuhVytieLLEAZgsVFGMnfJUAKJAiQp6ufiTASGwnCzmPbvUJPQBFTQEUlNPNCwBlGsBCeCBthQAokCJCnq5+JMBIbCUHkKLZ/GlJAEW2Gmpy7DEVAGTcY9RaMBmZACiQIkKern4kwEhsJ0SeWSXhQNEARRQrxJHl3XkAZ74zh1f0PZEA=
[Protobuf]> decoded: TimeframeData {
  dataPoints: [
    Timeframe { timestamp: [Long], values: [TimeframeValues] },
    Timeframe { timestamp: [Long], values: [TimeframeValues] },
    Timeframe { timestamp: [Long], values: [TimeframeValues] },
    Timeframe { timestamp: [Long], values: [TimeframeValues] },
    Timeframe { timestamp: [Long], values: [TimeframeValues] },
    Timeframe { timestamp: [Long], values: [TimeframeValues] },
    Timeframe { timestamp: [Long], values: [TimeframeValues] },
    Timeframe { timestamp: [Long], values: [TimeframeValues] },
    Timeframe { timestamp: [Long], values: [TimeframeValues] },
    Timeframe { timestamp: [Long], values: [TimeframeValues] }
  ]
}
```

### Test 5

Build time and bundle size:

```
yarn run v1.22.10
$ yarn build-test5-avro && yarn build-test5-bson && yarn build-test5-cbor && yarn build-test5-messagepack && yarn build-test5-protobuf && yarn build-test5-protobuf-compiled
$ esbuild test5/test5-avro.js --bundle --outfile=test5/test5-avro.bundle.js --target="chrome100,firefox99" --define:process.env={} --inject:test5/buffer-shim.js --minify

  test5\test5-avro.bundle.js  111.7kb

$ esbuild test5/test5-bson.js --bundle --outfile=test5/test5-bson.bundle.js --target="chrome100,firefox99" --minify

  test5\test5-bson.bundle.js  98.0kb

$ esbuild test5/test5-cbor.js --bundle --outfile=test5/test5-cbor.bundle.js --target="chrome100,firefox99" --minify

  test5\test5-cbor.bundle.js  30.1kb

$ esbuild test5/test5-messagepack.js --bundle --outfile=test5/test5-messagepack.bundle.js --target="chrome100,firefox99" --minify

  test5\test5-messagepack.bundle.js  27.7kb

$ esbuild test5/test5-protobuf.js --bundle --outfile=test5/test5-protobuf.bundle.js --target="chrome100,firefox99" --minify

  test5\test5-protobuf.bundle.js  76.6kb

$ yarn pbjs -t static-module -w commonjs -o test5/test5-protobuf-compiled-proto.js test5/test5.proto && esbuild test5/test5-protobuf-compiled.js --bundle --outfile=test5/test5-protobuf-compiled.bundle.js --target="chrome100,firefox99" --minify
$ node_modules\.bin\pbjs -t static-module -w commonjs -o test5/test5-protobuf-compiled-proto.js test5/test5.proto

  test5\test5-protobuf-compiled.bundle.js  30.0kb
```

And timings:

```
Chrome 101
---
Serializer|Encoding time|Decoding time
---
Avro	1ms	1ms
BSON	2ms	1ms
CBOR	1ms	1ms
MessagePack	1ms	1ms
Protobuf	5ms	0ms
Protobuf, compiled	0ms	0ms
---
Firefox 100
---
Avro 	3ms 	0ms
BSON 	2ms 	1ms
CBOR 	1ms 	0ms
MessagePack 	1ms 	1ms
Protobuf 	4ms 	1ms
Protobuf, compiled 	0ms 	0ms
```