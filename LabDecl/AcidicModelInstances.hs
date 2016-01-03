{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module LabDecl.AcidicModelInstances where

import LabDecl.Types
import LabDecl.AcidicModels

type family LookupByIdEvent i where
  LookupByIdEvent CcaId     = LookupCcaById
  LookupByIdEvent TeacherId = LookupTeacherById
  LookupByIdEvent SubjectId = LookupSubjectById
  LookupByIdEvent StudentId = LookupStudentById

type family AddEntityEvent i where
  AddEntityEvent Cca     = AddCca
  AddEntityEvent Teacher = AddTeacher
  AddEntityEvent Subject = AddSubject
  AddEntityEvent Student = AddStudent

type family RemoveEntityEvent i where
  RemoveEntityEvent Cca     = RemoveCca
  RemoveEntityEvent Teacher = RemoveTeacher
  RemoveEntityEvent Subject = RemoveSubject
  RemoveEntityEvent Student = RemoveStudent

class (HasPrimaryKey a i) => HasCRUDEvents a i where
  lookupByIdEvent :: i -> LookupByIdEvent i
  addEntityEvent :: Bool -> a -> AddEntityEvent a
  removeEntityEvent :: i -> RemoveEntityEvent a

instance HasCRUDEvents Cca CcaId where
  lookupByIdEvent = LookupCcaById
  addEntityEvent = AddCca
  removeEntityEvent = RemoveCca

instance HasCRUDEvents Teacher TeacherId where
  lookupByIdEvent = LookupTeacherById
  addEntityEvent = AddTeacher
  removeEntityEvent = RemoveTeacher

instance HasCRUDEvents Subject SubjectId where
  lookupByIdEvent = LookupSubjectById
  addEntityEvent = AddSubject
  removeEntityEvent = RemoveSubject

instance HasCRUDEvents Student StudentId where
  lookupByIdEvent = LookupStudentById
  addEntityEvent = AddStudent
  removeEntityEvent = RemoveStudent
