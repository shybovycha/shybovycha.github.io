---
layout: post
title: Microsoft' error messages cipher
date: '2015-01-09T21:43:12+01:00'
tags:
- programming
- rtfm
- fun
tumblr_url: http://shybovycha.tumblr.com/post/107622125006/microsoft-error-messages-cipher
---

I have a T-SQL trigger creation script, which runs OK:

```sql
CREATE TRIGGER data_modified ON Northwind.dbo.Customers FOR INSERT, UPDATE, DELETE
AS

declare @rows as int;
set @rows = @@ROWCOUNT;

IF @rows = 0
BEGIN
    print 'no rows were affected';
    return;
end

if exists(select * from inserted)
begin
    if exists(select * from deleted)
    begin
        print 'updated ' + @rows + ' rows';
    end
    else
    begin
        print 'inserted ' + @rows + ' rows';
    end
end
else
begin
    print 'deleted ' + @rows + ' rows';
end
```

Yet, when I run some `INSERT` query, I got an error saying:

```
Msg 245, Level 16, State 1, Procedure data_modified, Line 21
Conversion failed when converting the varchar value 'inserted ' to data type int.
```

Mysterious, isn't it? Let's dig in, shall we?

<!--more-->

Let's look onto the source of that trigger, at line 18:

```sql
USE [Northwind]
GO
/****** Object:  Trigger [dbo].[data_modified]    Script Date: 09.01.2015 18:18:14 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
    ALTER TRIGGER [dbo].[data_modified] ON [Northwind].[dbo].[Customers] FOR INSERT, UPDATE, DELETE
    AS

    declare @rows as int;
    set @rows = @@ROWCOUNT;

    IF @rows = 0
    BEGIN
        print 'no rows were affected';
        return;
    end

    if exists(select * from inserted)
    begin
        if exists(select * from deleted)
        begin
            print 'updated ' + @rows + ' rows';
        end
        else
        begin
            print 'inserted ' + @rows + ' rows';
        end
    end
    else
    begin
        print 'deleted ' + @rows + ' rows';
    end
```

Here's the error:

```sql
if exists(select * from inserted)
```

But wait, that can't be true!

The problem is a bit deeper, with the `@rows` variable:

```sql
print 'updated ' + @rows + ' rows';
```

while being declared as:

```sql
declare @rows as int;
```

It can not be printed right away, so it needs to be cast:

```sql
CREATE TRIGGER data_modified ON Northwind.dbo.Customers FOR INSERT, UPDATE, DELETE
AS

declare @rows as int;
declare @rows_s as varchar(10);
set @rows = @@ROWCOUNT;
set @rows_s = cast(@rows as varchar);

IF @rows = 0
BEGIN
    print 'no rows were affected';
    return;
end

if exists(select * from inserted)
begin
    if exists(select * from deleted)
    begin
        print 'updated ' + @rows_s + ' rows';
    end
    else
    begin
        print 'inserted ' + @rows_s + ' rows';
    end
end
else
begin
    print 'deleted ' + @rows_s + ' rows';
end
```

Try to guess where's your mistake, using that error message! 😉
