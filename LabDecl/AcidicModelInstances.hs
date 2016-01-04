{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module LabDecl.AcidicModelInstances where

import Data.Vector (Vector)

import LabDecl.Types
import LabDecl.AcidicModels

class HasPrimaryKey a i => HasCRUDEvents a i lookupEv removeEv addEv addManyEv | i -> lookupEv, a -> removeEv, a -> addEv, a -> addManyEv where
  lookupByIdEvent :: i -> lookupEv
  removeEntityEvent :: i -> removeEv
  addEntityEvent :: Bool -> a -> addEv
  addEntitiesEvent :: Bool -> Vector a -> addManyEv

instance HasCRUDEvents Cca CcaId LookupCcaById RemoveCca AddCca AddCcas where
  lookupByIdEvent = LookupCcaById
  removeEntityEvent = RemoveCca
  addEntityEvent = AddCca
  addEntitiesEvent = AddCcas

instance HasCRUDEvents Teacher TeacherId LookupTeacherById RemoveTeacher AddTeacher AddTeachers where
  lookupByIdEvent = LookupTeacherById
  removeEntityEvent = RemoveTeacher
  addEntityEvent = AddTeacher
  addEntitiesEvent = AddTeachers

instance HasCRUDEvents Subject SubjectId LookupSubjectById RemoveSubject AddSubject AddSubjects where
  lookupByIdEvent = LookupSubjectById
  removeEntityEvent = RemoveSubject
  addEntityEvent = AddSubject
  addEntitiesEvent = AddSubjects

instance HasCRUDEvents Student StudentId LookupStudentById RemoveStudent AddStudent AddStudents where
  lookupByIdEvent = LookupStudentById
  removeEntityEvent = RemoveStudent
  addEntityEvent = AddStudent
  addEntitiesEvent = AddStudents
