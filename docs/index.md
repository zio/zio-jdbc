---
id: index
title: "Introduction to ZIO JDBC"
sidebar_label: "ZIO JDBC"
---

_ZIO JDBC_ is a ZIO library for JDBC access, providing a small, unopinionated, and ergonomic foundation for directly interacting with JDBC.

- Idiomatic ZIO 2.0 interface to JDBC
- Secure, with protection against SQL-injection
- Fully integrated with core libraries including _ZIO Schema_, _ZIO Config_, _ZIO Logging_

_ZIO JDBC_ provides the following components:

 - A SQL interpolator to prevent SQL injection attacks
 - A connection pool powered by ZIO's async, resource-safe pool
 - Lightweight decoding values from result sets 
 - Lightweight encoding values into SQL fragments for `INSERT`
 - Integration with ZIO Schema, ZIO Config, and ZIO Logging
 - Built-in metrics
