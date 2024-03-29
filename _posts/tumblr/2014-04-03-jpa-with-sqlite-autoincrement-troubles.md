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
<p>Today i&rsquo;ve got an exception when JPA was trying to insert a new row to a table.</p>

<p>My model definition was like this:</p>

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

<p>And the table creation script was the following:</p>

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

<p>I faced some strange exceptions like <em>id could not be null</em>, <em>SQL error or missing database (near &ldquo;values&rdquo;: syntax error)</em>, etc.</p>

<p>The solution to this consists of two steps:</p>

<ul><li><strong>in database:</strong> remove the <strong>AUTOINCREMENT</strong> and <strong>NOT NULL</strong> annotations from the <strong>ID</strong> column as SQLite will automatically increment its value (in DB)</li>
<li><strong>in entity&rsquo; code:</strong> remove the <em>GenerationStrategyType</em> from the <strong>id</strong> member annotation</li>
</ul>
