;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER; Base: 10 -*-
#-Symbolics
;;(in-package sme::*the-user-package*)		;defPackage
(in-package "COMMON-LISP-USER")		;defPackage

;;;; Predicate language definition file
(sme:defPredicate have (entity entity) relation)
(sme:defPredicate in (entity entity) relation)
(sme:defPredicate by (entity entity) relation)
(sme:defPredicate father (entity entity) relation)
(sme:defPredicate respects (entity entity) relation)
(sme:defPredicate authority (entity entity) relation)
(sme:defPredicate first-time (entity entity) relation)
(sme:defPredicate reluctant (entity entity) relation)
(sme:defPredicate forget (entity entity) relation)
(sme:defPredicate together (entity entity) relation)
(sme:defPredicate is (entity entity) relation)
(sme:defPredicate far (entity entity) relation)

(sme:defPredicate verb (entity entity) relation :expression-type action)
(sme:defPredicate take (entity entity) relation :expression-type action)
(sme:defPredicate tell (entity entity entity) relation :expression-type action)

(sme:defPredicate happy (entity) attribute :expression-type emotion)
(sme:defPredicate sad (entity) attribute :expression-type emotion)

(sme:defPredicate young (entity) attribute)
(sme:defPredicate old (entity) attribute)

(sme:defPredicate Hero (entity) attribute :expression-type role)
(sme:defPredicate Villain (entity) attribute :expression-type role)
(sme:defPredicate Tester (entity) attribute :expression-type role)
(sme:defPredicate Prize (entity) attribute :expression-type role)
(sme:defPredicate FalseHero (entity) attribute :expression-type role)
(sme:defPredicate Other (entity) attribute :expression-type role)