{-*- mode: text; -*-}
{-# LANGUAGE QuasiQuotes #-}
module LabDecl.ErrMsg where

import Control.Lens
import Text.Shakespeare.Text (stext)

import LabDecl.Types


errSubjectAlreadyExists code thisSubjectName existingSubject = [stext|
  Cannot add new subject “#{thisSubjectName}” to the database because
  there is an existing subject with the same subject code (“#{code}”)
  with overlapping levels. The existing subject is named
  “#{existingSubject ^. subjectName}”.  You can override this check by
  choosing Force, in which case the subject will still be added, but
  will cause errors when adding a student if that student’s subject
  combination involves the code “#{code}”. |]

errSubjectsNotUniquelyDecodable thisSubjectName level = [stext| Cannot
  add new subject “#{thisSubjectName}” to the database because adding
  such a subject will cause the set of subject codes for Year #{level}
  to be non-uniquely decodable. You can override this check by
  choosing Force, in which case the subject will still be added, but
  may cause errors when adding a student if that student’s subject
  combination cannot be unambiguously decoded.  |]

errTeacherEmailAlreadyExists new old = [stext| Cannot add new teacher
  “#{new ^. teacherName}” to the database because there already exists
  a teacher with the same email address (#{old ^. teacherEmail}). You
  can override this check by choosing Force, in which case the teacher
  will still be added, but neither teacher will be able to login using
  the email address #{old ^. teacherEmail}. |]

errTeacherWitnessNameAlreadyExists new old = [stext| Cannot add new
  teacher “#{new ^. teacherName}” to the database because there
  already exists a teacher with the same witness name
  (#{old ^. teacherWitnessName}). You can override this check by
  choosing Force, in which case the teacher will still be added, but
  will cause errors when adding a student if that student’ declaration
  witnesser name is “#{old ^. teacherWitnessName}”.  |]

errStudentAlreadyExists new old = [stext| Cannot add new student
  “#{new ^. studentName}” to the database because there already exists
  a student “#{old ^. studentName}” with the same class and index
  number. You can override this check by choosing Force, in which case
  the student will still be added, but will cause errors when looking
  up the student by class and index number. |]
