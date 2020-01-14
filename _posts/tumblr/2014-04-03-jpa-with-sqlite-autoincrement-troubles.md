---
layout: post
title: JPA with SQLite autoincrement troubles
date: '2014-04-03T11:46:00+02:00'
tags:
- sqlite
- jpa
- java
tumblr_url: http://shybovycha.tumblr.com/post/81573029618/jpa-with-sqlite-autoincrement-troubles
---

Today I've got an exception when JPA was trying to insert a new row to a table.

My model definition was like this:

```java
@Entity
public class Merchant {
    @Id
    @GeneratedValue(strategy=GenerationType.IDENTITY)
    private int id;
    private String name;

    // ...

    public Merchant(){   }

    // ...
```

And the table creation script was the following:

```sql
CREATE TABLE MERCHANT
(
    ID INTEGER PRIMARY KEY NOT NULL,
    NAME TEXT NOT NULL,
    BANKNAME TEXT NOT NULL,
    SWIFT TEXT NOT NULL,
    ACCOUNT TEXT NOT NULL,
    CHARGE REAL NOT NULL,
    PERIOD INTEGER NOT NULL,
    MINSUM REAL NOT NULL,
    TOTAL REAL NOT NULL
);
```

I faced some strange exceptions like `id could not be null`, `SQL error or missing database (near 'values': syntax error)`, etc.

The solution to this consists of two steps:

* **in database:** remove the `AUTOINCREMENT` and `NOT NULL` constraints from the `ID` column as SQLite will automatically increment its value (in the database)
* **in entity code:** remove the `GenerationStrategyType` from the `id` member annotation
