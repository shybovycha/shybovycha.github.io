---
layout: post
title: 'Message compression formats in a web application'
date: '2022-10-25T00:15:00+00:00'
---

I have seen quite a few web applications which are passing heaps of data between client and server. Pretty much all of them use JSON for that purpose.
And whilst that solves the business problem, some of those applications aim to provide nearly real-time user experience, and that's where the issues arise.

With the rise of web sockets, RTC and HTTP2 tech, the data transfer speed issues might be not so noticeable, but the more data app tries to transfer, the more
pressing the matter gets. This issue also becomes more apparent on the server side - server has to process a lot more data in a request thread.

At The Trade Desk I have observed an approach, where we are transferring a list of strings (later we converted them to integers to widen the bandwidth) between
high-loaded services (think 15 million requests per second with a hard time bound on request processing of 100 milliseconds).

I thought this is a cool concept, worth exploring deeper. For this matter, I have searched the Internet for similar approaches.

One of the readings was an [article](https://habr.com/ru/company/vk/blog/594633/) on the russian
resource about speeding up a web application by switching from HTTP1 to HTTP2, utilizing various data compression formats: for static assets - WEBP, AVIF and
progressive JPEG; for general data - various stream compression algorithms - GZIP, Brotli and ZSTD; alongside with different data serialization formats - MessagePack.

I decided to expand my search in the direction of serialization formats. In this article I compare few of those and focus on their performance in serializing
different kinds of data (lists, nested objects, integers and strings, large and small datasets)
and the serialization & deserialization performance in browser and the compression rates.

The formats covered are:

* [BSON](https://www.mongodb.com/json-and-bson)
* [CBOR](https://cbor.io/)
* [MessagePack](https://msgpack.org/)
* [Protobuf](https://developers.google.com/protocol-buffers/)
* [Thrift](https://github.com/apache/thrift)
* [Avro](https://github.com/apache/avro)
* [Cap'n'Proto](https://capnproto.org/)
* [FlatBuffers](https://github.com/google/flatbuffers)

One of the optimizations we have achieved at The Trade Desk was 7 times reduction in data size by switching from string IDs to integer values.
Think `"something-else"` we used to operate everywhere was assigned an integer ID of `4092`.
Now that int is 4 bytes long, whereas the string value is 14 bytes long. That is more than 3x reduction in size, but you have to still store the mapping
somewhere, which we already did (in our case).

But the overall idea is worth researching too - how much compression each format provides when working with long lists of strings and long lists of ints.

To make the thing more interesting, I came up with few various data types to be messed around:

* a simple object `Pet { name: string, kind: enum Kind { CAT, DOG } }`
* an object with an array of string identifiers `StringIdsResource { ids: string[] }`
* an object with an array of integer identifiers `StringIdsResource { ids: int[] }`

As mentioned before, the metrics I'm going to be focusing on are:

* encoding & decoding performance in browser environment
* encoded message length (raw, as UTF-8 string and base-64 encoded UTF-8 string)
* amount of runtime (bundled code) required to work with the format

To make it quick and easy, here's the summary _(time measured in browser, on a nested object with a list of `1000` children of various data types)_:

| Serializer         | Encoding time | Decoding time | Encoded data size (byte array) | Data saving | Encoded data size (base-64 utf-8 encoded) | Bundle size |
| ------------------ | ------------- | ------------- | ------------------------------ | ----------- | ----------------------------------------- | ----------- |
| Avro               | 12ms          | 4ms           | 30003                          | 77.16%      | 40004                                     | 111.7kb     |
| BSON               | 10ms          | 11ms          | 98912                          | 24.71%      | 131884                                    | 98.0kb      |
| CBOR               | 3ms           | 4ms           | 89017                          | 32.24%      | 118692                                    | 30.1kb      |
| MessagePack        | 3ms           | 3ms           | 89017                          | 32.24%      | 118692                                    | 27.7kb      |
| Protobuf           | 13ms          | 3ms           | 38000                          | 71.07%      | 50668                                     | 76.6kb      |
| Protobuf, compiled | 6ms           | 1ms           | 38000                          | 71.07%      | 50668                                     | 30.0kb      |
| Flatbuffers        | 9ms           | 3ms           | 32052                          | 75.60%      | 42736                                     | 3.1kb       |
| Thrift (binary)    | 42ms          | 6ms           | 45009                          | 65.74%      | 60012                                     | 109.7kb     |
| Thrift (compact)   | 33ms          | 11ms          | 36005                          | 72.59%      | 48008                                     | 109.7kb     |

Few learnings:

* Thrift is quite slow and not that straightforward to use (after all, it was designed to provide entire communication layer for an application), but provides decent compression rate
* Protobuf and Avro provide by far the most compact output (because of schema provided)
* Protobuf library (protobufjs), unlike Avro, can't handle enumerations (Protobufjs requires raw integer values to be used whereas Avro supports semantic, string values)
* Cap'n'Proto seems outdated and its JS plugin did not get any support for few years now, have to check the TS version
* FlatBuffers is quite low-level and tricky to use (much more effort than those other tools)

My take on these results is that:

* Protobuf, when using a compiled serialzier/deserializer for specific message(-s):
  * has a comfortable API
  * fast serialization / deserialization in browser
  * decent compression rate
  * relatively small bundle size increase
* Flatbuffers:
  * great compression rate
  * tiny bundle size impact
  * great performance in browser
  * super-cumbersome API (well, that's low-level trade-offs for ya)

The source code for the tests could be found on my [GitHub repo](https://github.com/shybovycha/webapp-data-serialization-formats-comparison).

Under the cut you will find a stream of consciousness - my notes whilst implementing the tests for these tech.

<!--more-->

## Preface

Some of the serialization tools listed in this blog require message (or rather data) schema to be defined beforehand.
This might seem like unnecessary extra work, but if implemented correctly, it has quite a few benefits:

* no need to ship an entire runtime / parser
* type checking even in weakly typed languages
* contract enforcing becomes an option, no real need for contract testing

Amongst technologies covered here, only three do not use data schema:

* CBOR
* BSON
* MessagePack

But the others, which do use schemas, need the schema to be compiled before it is used.

### Avro

First time I've heard about Avro is when I had to work with Kafka - it was used to enforce message format across queues.

With Avro, one does not really _have_ to compile schema before it is used (although it is possible) - schema can be defined at runtime:

```js
const avro = require('avsc/etc/browser/avsc-types');

const AvroType = avro.Type.forSchema({
  type: 'record',
  name: 'TimeframeResource',
  fields: [
    {
      name: 'dataPoints',
      type: {
        type: 'array',
        items: {
          type: 'record',
          name: 'DataPoint',
          fields: [
            {
              name: 'timestamp',
              type: 'long',
            },

            {
              name: 'values',
              type: {
                type: 'record',
                name: 'TimeframeValues',
                fields: [
                  {
                    name: 'category1',
                    type: 'double',
                  },

                  {
                    name: 'category2',
                    type: 'double',
                  },

                  {
                    name: 'category3',
                    type: 'double',
                  },
                ],
              },
            },
          ],
        },
        default: [],
      },
    },
  ],
});
```

The only trick with Avro is that `long` is not really a type, for whatever reason.
The 64-bit wide integers are very useful when working with `DateTime` to reduce the message size (consider `31-Oct-2022T09:34:00+10:00` (26 characters = 26 bytes) vs `1667172840` (64 bit = 2 bytes)).
And the way to define this type in Avro is a bit quirky:

```js
const LongType = avro.types.LongType.__with({
    fromBuffer: (buf) => buf.readBigInt64LE(),
    toBuffer: (n) => {
        const buf = Buffer.alloc(8);
        buf.writeBigInt64LE(BigInt(n));
        return buf;
    },
    fromJSON: Number,
    toJSON: Number,
    isValid: (n) => typeof n == 'number',
    compare: (n1, n2) => { return n1 === n2 ? 0 : (n1 < n2 ? -1 : 1); }
});

const AvroType = avro.Type.forSchema(
  {
    /* schema */
  },
  {
    registry: { 'long': LongType }
  }
);
```

<div class="content-read-marker" data-fraction="25"></div>

But then, serialization and deserialization are extremely simple:

```js
const data = {
  dataPoints: [
    {"timestamp":new Date("2022-05-05T21:11:22+10:00").getTime(),"values":{"category1":-5.207119058209394,"category2":89.29685288758918,"category3":35.90829865270196}},
    {"timestamp":new Date("2022-05-05T21:11:22+10:00").getTime(),"values":{"category1":-43.35796609790218,"category2":21.846789565420153,"category3":124.58032201029741}},
  ],
};

const buf = AvroType.toBuffer(data);

const obj = AvroType.fromBuffer(buf);
```

### BSON

BSON is a schema-less library, so usage is as straightforward as it can be:

```js
const BSON = require('bson');

const data = {
  dataPoints: [
    {"timestamp":new Date("2022-05-05T21:11:22+10:00").getTime(),"values":{"category1":-5.207119058209394,"category2":89.29685288758918,"category3":35.90829865270196}},
    {"timestamp":new Date("2022-05-05T21:11:22+10:00").getTime(),"values":{"category1":-43.35796609790218,"category2":21.846789565420153,"category3":124.58032201029741}},
  ],
};

const buf = BSON.serialize(data);

const obj = BSON.deserialize(buf);
```

### CBOR

CBOR is very similar to BSON - it is a schema-less tool, so it is as straightforward as it gets:

```js
const CBOR = require('cbor-x');

const data = {
  dataPoints: [
    {"timestamp":new Date("2022-05-05T21:11:22+10:00").getTime(),"values":{"category1":-5.207119058209394,"category2":89.29685288758918,"category3":35.90829865270196}},
    {"timestamp":new Date("2022-05-05T21:11:22+10:00").getTime(),"values":{"category1":-43.35796609790218,"category2":21.846789565420153,"category3":124.58032201029741}},
  ],
};

const buf = CBOR.encode(data);

const obj = CBOR.decode(buf);
```

### MessagePack

MessagePack is the last one of the schema-less tools, so it is once again as straightforward as one might think:

```js
const MessagePack = require('msgpackr');

const data = {
  dataPoints: [
    {"timestamp":new Date("2022-05-05T21:11:22+10:00").getTime(),"values":{"category1":-5.207119058209394,"category2":89.29685288758918,"category3":35.90829865270196}},
    {"timestamp":new Date("2022-05-05T21:11:22+10:00").getTime(),"values":{"category1":-43.35796609790218,"category2":21.846789565420153,"category3":124.58032201029741}},
  ],
};

const buf = MessagePack.pack(data);

const obj = MessagePack.unpack(buf);
```

### Protobuf

With Protobuf, the schema can be defined and parsed in runtime:

```js
const protobuf = require('protobufjs');

const ProtobufProto = `syntax = "proto3";

package testpackage;

message TimeframeValues {
    double category1 = 1;
    double category2 = 2;
    double category3 = 3;
}

message Timeframe {
    int64 timestamp = 1;
    TimeframeValues values = 2;
}

message TimeframeData {
    repeated Timeframe dataPoints = 1;
}
`;

const ProtobufType = protobuf.parse(ProtobufProto).root.lookupType('testpackage.TimeframeData');
```

With parsing, one has to lookup the message type from the "root" type, before it can be used for serialization.

The schema can also be compiled as a separate build step:

```sh
yarn pbjs -t static-module -w commonjs -o ProtobufType.js ProtobufType.proto
```

The serialization and deserialization then become quite straightforward:

```js
const data = {
  dataPoints: [
    {"timestamp":new Date("2022-05-05T21:11:22+10:00").getTime(),"values":{"category1":-5.207119058209394,"category2":89.29685288758918,"category3":35.90829865270196}},
    {"timestamp":new Date("2022-05-05T21:11:22+10:00").getTime(),"values":{"category1":-43.35796609790218,"category2":21.846789565420153,"category3":124.58032201029741}},
  ],
};

const buf = ProtobufType.encode(data).finish();

const obj = ProtobufType.decode(buf);
```

### FlatBuffers

Defining schema with FlatBuffers looks like this:

```
namespace testpackage;

table TimeframeValues {
  category1: float32;
  category2: float32;
  category3: float32;
}

table Timeframe {
  timestamp: int64;
  values: TimeframeValues;
}

table TimeframeData {
  dataPoints: [Timeframe];
}

root_type TimeframeData;
```

The schema is then compiled using the following command:

```sh
flatc -o flatbuffers-compiled-proto/ --ts TimeframeData.fbs
```

One might think this is how to encode an object with FlatBuffers (which is a cumbersome low-level API already):

```js
const builder = new flatbuffers.Builder(0);

FlatbuffersTimeframeData.TimeframeData.startTimeframeData(builder);
FlatbuffersTimeframeData.TimeframeData.startDataPointsVector(builder, dataPoints.length);

dataPoints.forEach(({ timestamp, values: { category1, category2, category3 } }) => {
    FlatbuffersTimeframe.Timeframe.startTimeframe(builder);

        FlatbuffersTimeframe.Timeframe.addTimestamp(timestamp);

        FlatbuffersTimeframeValues.TimeframeValues.startTimeframeValues(builder);

            FlatbuffersTimeframeValues.TimeframeValues.addCategory1(builder, category1);
            FlatbuffersTimeframeValues.TimeframeValues.addCategory2(builder, category2);
            FlatbuffersTimeframeValues.TimeframeValues.addCategory3(builder, category3);

            // alternatively: FlatbuffersTimeframeValues.TimeframeValues.createTimeframeValues(builder, category1, category2, category3);

        const vs = FlatbuffersTimeframeValues.TimeframeValues.endTimeframeValues(builder);

        FlatbuffersTimeframe.Timeframe.addValues(builder, vs);

    const tf = FlatbuffersTimeframe.Timeframe.endTimeframe();

    FlatbuffersTimeframeData.TimeframeData.addDataPoints(builder, tf);
});

const offset = FlatbuffersTimeframeData.TimeframeData.endTimeframeData(builder);

builder.finish(offset);

return builder.asUint8Array();
```

<div class="content-read-marker" data-fraction="50"></div>

But this won't do - an error would be thrown:

```
Uncaught Error: FlatBuffers: object serialization must not be nested.
```

With FlatBuffers serialization is like managing the memory in C98 - you first allocate the memory, then you fill it with data, then you use it elsewhere:

```js
const builder = new flatbuffers.Builder(0);

const tfs = dataPoints.map(({ timestamp, values: { category1, category2, category3 } }) => {
    const vs = FlatbuffersTimeframeValues.TimeframeValues.createTimeframeValues(builder, category1, category2, category3);

    FlatbuffersTimeframe.Timeframe.startTimeframe(builder);

        FlatbuffersTimeframe.Timeframe.addTimestamp(builder, timestamp);
        FlatbuffersTimeframe.Timeframe.addValues(builder, vs);

    return FlatbuffersTimeframe.Timeframe.endTimeframe(builder);
});

const dps = FlatbuffersTimeframeData.TimeframeData.createDataPointsVector(builder, tfs);

FlatbuffersTimeframeData.TimeframeData.startTimeframeData(builder);
FlatbuffersTimeframeData.TimeframeData.addDataPoints(builder, dps);

const offset = FlatbuffersTimeframeData.TimeframeData.endTimeframeData(builder);

builder.finish(offset);

return builder.asUint8Array();
```

Deserializing objects is also not all that similar with the other tech in this list - accessing each property is done via methods provided by the generated proto classes:

```js
const buf = new flatbuffers.ByteBuffer(data);

const timeframeData = FlatbuffersTimeframeData.TimeframeData.getRootAsTimeframeData(buf);

// accessing dataPoints: timeframeData.dataPoints(timeframeData.dataPointsLength() - 1).values().category1()
```

This aspect makes FlatBuffers not so friendly to use. Meaning if you decide to incorporate it in your project, not only will you need to compile the schemas separately, but
the effort to implement serialization might become a decisive factor against it, compared to other tools in this review.

### Thrift

Schema for Thrift looks like this:

```thrift
struct TimeframeValues {
  1: required double category1;
  2: required double category2;
  3: required double category3;
}

struct Timeframe {
  1: required i32 timestamp;
  2: required TimeframeValues values;
}

struct TimeframeData {
  1: required list<Timeframe> dataPoints;
}
```

Compiling it uses tool `thrift-typescript` (for TypeScript & JS compilation):

```sh
yarn thrift-typescript --outDir ./thrift-compiled-proto --rootDir . --sourceDir . TimeframeData.thrift
```

Thrift, similarly to FlatBuffers is a bit tricky to get running. First, it needs a _protocol_ and a _transport_ to operate.
There are two protocols, `TBinaryProtocol`, which provides some level of compression and `TCompactProtocol`, which offers more compression.
Then, similarly to FlatBuffers, the nested objects have to be serialized first.
And finally, the API is not very user friendly - one might think using the constructors is sufficient, but in fact one has to rely on callbacks:

This won't do:

```js
const buf = new Buffer();
const thriftTransport = new thrift.TBufferedTransport(buf);
const binaryThriftProtocol = new thrift.TCompactProtocol(thriftTransport);

const obj = new ThriftType.TimeframeData(data);

obj.write(binaryThriftProtocol);

binaryThriftProtocol.flush();

return buf;
```

Instead, one should use callback API and serialize nested objects first:

```js
let thriftBuffer = null;

const thriftTransport = new thrift.TBufferedTransport(null, res => thriftBuffer = res);
const binaryThriftProtocol = new thrift.TCompactProtocol(thriftTransport);

const dataPoints = data.dataPoints.map(dp => {
  const vs = new ThriftType.TimeframeValues(dp.values);

  return new ThriftType.Timeframe({ timestamp: dp.timestamp, values: vs });
});

const obj = new ThriftType.TimeframeData({ dataPoints });

obj.write(binaryThriftProtocol);

binaryThriftProtocol.flush();

return thriftBuffer;
```

Deserialization is also messed up by these low-level APIs:

```js
let obj = null;

const tr = thrift.TBufferedTransport.receiver(transport => {
  const protocol = new thrift.TCompactProtocol(transport);

  obj = ThriftType.TimeframeData.read(protocol);
});

tr(Buffer.from(data));

return obj;
```

## Serialization output

Let's consider serializing & deserializing a simple object of type `Pet`:

```ts
enum PetKind {
  CAT,
  DOG,
}

interface Pet {
  name: string;
  kind: PetKind;
}
```

And the object for the experiments:

```js
{ name: 'Rodrigo', kind: 'DOG' }
```

First, let's take a look at schema-less serialization libraries - CBOR, BSON and MessagePack:

### CBOR

```
CBOR-encode: 0.779ms
CBOR-decode: 0.587ms
[CBOR]> pre-utf8 (25): <Buffer b9 00 02 64 6e 61 6d 65 67 52 6f 64 72 69 67 6f 64 6b 69 6e 64 63 44 4f 47, dataView: DataView { byteLength: 25, byteOffset: 0, buffer: ArrayBuffer { [Uint8Contents]: <b9 00 02 64 6e 61 6d 65 67 52 6f 64 72 69 67 6f 64 6b 69 6e 64 63 44 4f 47 00 06 00 05 00 04 00 0f 05 00 0f 10 04 00 04 00 25 c6 58 36 01 00 00 01 00 00 00 10 3e 58 0f 01 ff ff ff 31 00 00 00 c8 2e c6 58 36 01 00 00 09 00 00 00 00 ce 65 1c 01 3c c6 58 36 01 00 00 00 00 00 00 00 00 00 00 b0 80 cc 58 ... 8092 more bytes>, byteLength: 8192 } }>
[CBOR]> post-utf8 (23): 适dnamegRodrigodkindcDOG
[CBOR]> base-64 (36): uQACZG5hbWVnUm9kcmlnb2RraW5kY0RPRw==
[CBOR]> decoded: { name: 'Rodrigo', kind: 'DOG' }
```

<div class="content-read-marker" data-fraction="75"></div>

### BSON

```
BSON-encode: 0.631ms
BSON-decode: 1.079ms
[BSON]> pre-utf8 (37): <Buffer 25 00 00 00 02 6e 61 6d 65 00 08 00 00 00 52 6f 64 72 69 67 6f 00 02 6b 69 6e 64 00 04 00 00 00 44 4f 47 00 00>
[BSON]> post-utf8 (37): %☻namRodrigo☻kind♦DOG
[BSON]> base-64 (52): JQAAAAJuYW1lAAgAAABSb2RyaWdvAAJraW5kAAQAAABET0cAAA==
[BSON]> decoded: { name: 'Rodrigo', kind: 'DOG' }
```

### MessagePack

```
MessagePack-encode: 0.738ms
MessagePack-decode: 0.664ms
[MessagePack]> pre-utf8 (25): <Buffer de 00 02 a4 6e 61 6d 65 a7 52 6f 64 72 69 67 6f a4 6b 69 6e 64 a3 44 4f 47, dataView: DataView { byteLength: 25, byteOffset: 0, buffer: ArrayBuffer { [Uint8Contents]: <de 00 02 a4 6e 61 6d 65 a7 52 6f 64 72 69 67 6f a4 6b 69 6e 64 a3 44 4f 47 00 06 00 05 00 04 00 0f 05 00 0f 10 04 00 04 00 25 c6 58 36 01 00 00 01 00 00 00 10 3e 58 0f 01 ff ff ff 31 00 00 00 28 2e c6 58 36 01 00 00 08 00 00 00 00 ce 65 1c 01 fc c5 58 36 01 00 00 b0 2e c6 58 36 01 00 00 0c 00 00 00 ... 8092 more bytes>, byteLength: 8192 } }>
[MessagePack]> post-utf8 (16): ހ☻䮡me璯drigo䫩ndㄏG
[MessagePack]> base-64 (36): 3gACpG5hbWWnUm9kcmlnb6RraW5ko0RPRw==
[MessagePack]> decoded: { name: 'Rodrigo', kind: 'DOG' }
```

Except for MessagePack, which seem to utilize the tight byte-packing, the idea of these tools is to put field name before the field value and just pack the data bytes as tightly as possible.

Now, for the schema-enforced libraries:

### Avro

```
Avro-encode: 0.199ms
Avro-decode: 0.181ms
[Avro]> pre-utf8 (9): <Buffer 02 0e 52 6f 64 72 69 67 6f>
[Avro]> post-utf8 (9): ☻♫Rodrigo
[Avro]> base-64 (12): Ag5Sb2RyaWdv
[Avro]> decoded: Pet { kind: 'DOG', name: 'Rodrigo' }
```

### Protobuf

```
Protobuf-encode: 2.723ms
Protobuf-decode: 0.231ms
[Protobuf]> pre-utf8 (11): <Buffer 08 00 12 07 52 6f 64 72 69 67 6f>
[Protobuf]> post-utf8 (11):↕Rodrigo
[Protobuf]> base-64 (16): CAASB1JvZHJpZ28=
[Protobuf]> decoded: Pet { kind: 0, name: 'Rodrigo' }
```

Since there is no need to transfer field names and field types are already known, the big difference with the schema-enforced libraries is that there is only need to send the start & end markers of the object' data. Hence the big difference in message size.

## Build time and bundle size

Since different tools require different stuff to run (like schema & message parsers, validators, etc.) and might require a separate build step to compile schemas, it might be valuable to know what you are dealing with.

Consider the data type used in the previous experiments:

```ts
interface TimeframeValues {
  category1: double;
  category2: double;
  category3: double;
}

interface Timeframe {
  timestamp: long;
  values: TimeframeValues;
}

interface TimeframeData {
  dataPoints: Timeframe[];
}
```

| Tech                       | Bundle size |
| -------------------------- | ----------- |
| Avro                       | 111.7kb     |
| BSON                       | 98.0kb      |
| CBOR                       | 30.1kb      |
| MessagePack                | 27.7kb      |
| Protobuf (parse schema)    | 76.6kb      |
| Protobuf (compiled schema) | 30.0kb      |
| Flatbuffers                | 3.1kb       |
| Thrift (compiled schema)   | 109.7kb     |

All of the above were build with TypeScript first and esbuild then with `--minify` option.

## Conclusion

As for the best technology out of the ones reviewed here, I think **Protobuf** is the most viable one - the data compression (71% on mixed data), the bundle size (+30kb), the serialization (6ms) & deserialization (1ms) times result in the best overall ratio out there.

If your project needs to optimize on the transferred data amount without suffering from slowdowns (due to serialization & deserialization) or bigger client JS bundle, Protobuf is the way to go.

Do not even consider BSON, CBOR and MessagePack - at the same bundle size increase, they give very little data compression, so optimization would be pretty much pointless. They do serialize the data faster (6ms with Protobuf vs 3ms with CBOR or MessagePack), but they also deserialize the data slower (lowest is 3ms with MessagePack vs 1ms with Protobuf).

<div class="content-read-marker" data-fraction="100"></div>
