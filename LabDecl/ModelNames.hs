{-# LANGUAGE TemplateHaskell #-}
module LabDecl.ModelNames (eventNames) where

import Language.Haskell.TH.Syntax (Name)

import LabDecl.Models

-- | The exported update/query event names. These events will be made
-- acidic, and they do not contain type variables.
eventNames :: [Name]
eventNames = [
    'listNothing,
    'listCcas,
    'listSubjects,
    'listTeachers,
    'listStudents,
    'lookupCcaById,
    'lookupSubjectById,
    'lookupTeacherByEmail,
    'lookupTeacherByWitnessName,
    'lookupTeacherById,
    'lookupSubjectByCodeLevel,
    'lookupStudentById,
    'lookupStudentByClassIndexNumber,
    'listSubjectsByLevel,
    'listStudentsFromClass,
    'listStudentsFromCca,
    'listStudentsWithSubject,
    'listStudentsWithWitnesser,
    'listStudentsByStatus,
    'listStudentsByLevel,
    'searchStudentsByName,
    'addCca,
    'addSubject,
    'addTeacher,
    'addStudent,
    'addCcas,
    'addTeachers,
    'addSubjects,
    'addStudents,
    'replaceCca,
    'replaceSubject,
    'replaceTeacher,
    'replaceStudent,
    'removeCca,
    'removeSubject,
    'removeTeacher,
    'removeStudent,
    'removeCcas,
    'removeSubjects,
    'removeTeachers,
    'removeStudents,
    'publicListClasses,
    'publicListStudentsFromClass,
    'publicLookupStudentByClassIndexNumber,
    'publicStudentDoSubmission,
    'publicStudentSubmissionPdfRendered,
    'teacherChangeSubmissionStatus,
    'teacherChangeManySubmissionStatus
    ]
