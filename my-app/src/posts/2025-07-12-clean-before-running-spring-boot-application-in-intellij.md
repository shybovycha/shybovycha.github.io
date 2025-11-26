---
layout: post
title: 'Clean before running a Spring Boot application in IntelliJ Idea'
---

My teammate has faced an issue when running our SpringBoot application in IntelliJ Idea: there were certain Gradle artifacts cached between switching the branches, which prevented the application from running. The issue is easily solved by running `clean` Gradle task. But my teammate was running it using IntelliJ's UI. And by default, it does not do a _clean_ build before running - just the normal build, if the files have changed.

The solution was a bit of UI trickery.

In the run menu, edit the run configuration (from the meatball menu):

<img src="/images/clean-before-run-spring-boot-application-in-intellij/screenshot1.png" />

From the run configuration window, click the "Modify options" link:

<img src="/images/clean-before-run-spring-boot-application-in-intellij/screenshot2.png" />

In the "Before launch" section of a very long drop-down menu, click the "Add before launch task":

<img src="/images/clean-before-run-spring-boot-application-in-intellij/screenshot3.png" />

From the "Add task" pop-up menu select "Run Gradle task":

<img src="/images/clean-before-run-spring-boot-application-in-intellij/screenshot4.png" />

In the Gradle tak window fill out the "Gradle project" and "Tasks" fields (they both have auto-complete):

<img src="/images/clean-before-run-spring-boot-application-in-intellij/screenshot5.png" />

Bear in mind: this advice might become obsolete as IntelliJ team changes their new UI.

