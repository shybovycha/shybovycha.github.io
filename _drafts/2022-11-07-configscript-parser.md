---
layout: post
title: "ConfigScript"
date: "07-11-2022T00:00:00+10:00"
---

Once upon a time I thought OGRE was overly too complicated - all those unnecessary script files, custom formats, a ton of setup hassle.
That was until I tried figuring out an entire modern 3D rendering stack from scratch.
That's when I realized configuring a rendering process (rendering pipeline) can be tricky.
And that's when I realized configuring application with config files can be helpful.
OGRE suddenly became very appealing to me and I really began to appreciate all the work the devs have put into it.

One good aspect of OGRE was the "unnecessary" script and configuration files. But the syntax of those files looked much cleaner
than that of JSON:

```
// This is a comment
object_keyword Example/ObjectName
{
    attribute_name "some value"

    object_keyword2 "Nested Object"
    {
        other_attribute 1 2 3
        // and so on..
    }
}
```

I thought if I could harvest anything from OGRE into my application with OpenGL, that would be the configuration based on this format (rather than Lua or whatever scripts).

Hence I crafted this simple grammar in ANTLR4 to parse these files:

```antlr
grammar ConfigScript;

config : (object)* ;

object : identifier WHITESPACE stringValue WHITESPACE '{' (property)* '}' ;

identifier
    : '\'' rawIdentifier '\''
    | '"' rawIdentifier '"'
    | rawIdentifier
    ;

rawIdentifier : ALPHA rawIdentifier1 ;

rawIdentifier1
    : ALPHA rawIdentifier1
    | NUM rawIdentifier1
    | DASH rawIdentifier1
    ;

property : (WHITESPACE)* rawIdentifier (WHITESPACE)+ propertyValue ;

propertyValue
    : propertyValue1
    | propertyValue1 (WHITESPACE)+ propertyValue
    ;

propertyValue1
    : integerValue
    | floatValue
    | booleanValue
    | stringValue
    ;

integerValue : ('+'|'-') (NUM)+ ;

floatValue : ('+'|'-') (NUM)+ '.' (NUM)+ ;

booleanValue : ('true' | 'false') ;

stringValue
    : '"' doubleQuotedRawString '"'
    | '\'' singleQuotedRawString '\''
    ;

doubleQuotedRawString : (NODOUBLEQUOTE)* ;
singleQuotedRawString : (NOSINGLEQUOTE)* ;

WHITESPACE : [ \r\n\t]+ -> skip;
ALPHA : [a-zA-Z] ;
NUM : [0-9] ;
DASH : '-' ;

NODOUBLEQUOTE : [^"] ;
NOSINGLEQUOTE : [^'] ;
```
