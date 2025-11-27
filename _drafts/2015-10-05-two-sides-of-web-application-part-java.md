---
layout: post
title: Two sides of web application. Part Java
categories: []
tags: [java, spring-boot, maven, backend, tutorial]
published: True
---

## Spring Boot

Initialize Maven project:

```bash
mvn archetype:generate -DgroupId=com.mycompany.app -DartifactId=my-app -DarchetypeArtifactId=maven-archetype-quickstart -DinteractiveMode=false
```

Add Spring Boot to `pom.xml`:

```xml
<build>
    <plugins>
        <plugin>
            <groupId>org.springframework.boot</groupId>
            <artifactId>spring-boot-maven-plugin</artifactId>
        </plugin>
    </plugins>
</build>

<parent>
    <groupId>org.springframework.boot</groupId>
    <artifactId>spring-boot-starter-parent</artifactId>
    <version>1.2.6.RELEASE</version>
</parent>

<dependencies>
    <dependency>
        <groupId>org.springframework.boot</groupId>
        <artifactId>spring-boot-starter-web</artifactId>
    </dependency>
</dependencies>
```

Development run: `mvn spring-boot:run`
Build project: `mvn clean install package`
Production run: `java -jar target/my-app-1.0-SNAPSHOT.jar`

Application servers: `resin`, `jlupin`