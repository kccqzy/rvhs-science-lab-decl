{-*- mode: text; -*-}
{-# LANGUAGE QuasiQuotes #-}
module LabDecl.ErrMsg where

import Control.Lens
import qualified Data.Text as T
import qualified Data.Set as Set
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

errEntityReferencedByStudent typeName students entityName = [stext|
  Cannot delete the #{typeName} “#{entityName}” because there is a
  student named “#{name}” whose #{typeName} is
  “#{entityName}”. Deleting will cause the student’s #{typeName} to be
  inconsistent. You must delete this student or delete this subject
  from this student.  |]
  where name = (^. studentName) . head . Set.toList $ students

errEntityReferencedByManyStudents typeName students entityName =
  [stext| Cannot delete the #{typeName} “#{entityName}” because there
  are #{Set.size students} students (including “#{name}” and
  #{Set.size students - 1} more) whose #{typeName} is
  “#{entityName}”. Deleting will cause these students’ #{typeName} to
  be inconsistent. You must delete those students or delete this
  subject from those students.  |]
  where name = (^. studentName) . head . Set.toList $ students

errEntityReferencedByStudents typeName students =
  (if Set.size students == 1
   then errEntityReferencedByStudent
   else errEntityReferencedByManyStudents) typeName students

errEntityNotExist what = [stext| The #{whatName} referenced in the
  request does not exist. If you’re using the GUI, this is an internal
  error. If you’re using the JSON REST API, check the id again. |]
  where whatName = T.toLower . T.pack . show $ what

errInvalidPublicSubmission = [stext| “You don’t try to game the
  system!” Mrs Look shouts. Uh-oh. ;( But then of course, every
  teenager has some anti-authority, anti-establishment streak. So it’s
  perfectly okay and understandable. Even Steve Jobs hacked the public
  telephone system when he was young. I, Qian Zhouyu of class 6N of
  2015, the builder of this website once hacked the school Ntrix
  system. Of course I’m not comparing myself to the great Jobs, but my
  point is, I believe every human being has an anti-establishment
  heart deep within him/her, though that may be burried deep by years
  and years of experience in the harsh reality. The sharp angles
  turned obtuse, the undaunted passion turned numb. Only in the
  above-average ones among us does it still manifest. Fortunately, you
  are one of them: you have kept your angles, had both the curiosity
  and the skills to pull this feat. I am impressed; I am in awe. I’d
  like to know you as a friend, talk to you—we might be great friends
  after this! And I mean it: there aren’t that many students (in RV)
  that has enough working knowledge of HTML, JavaScript, JSON, HTTP to
  achieve it. Email me at qzy@qzy.io or find me on Twitter @kccqzy. |]

errCSVDecodeFailed = [stext| Cannot decode the uploaded CSV file. You
  may wish to open the CSV file in Notepad, TextEdit or another
  plain-text editing utility to visually inspect the structure of the
  file.  |]

errCSVTextDecodeFailed = [stext| Cannot decode the uploaded CSV file
  using available text encodings. A list of common text encodings
  (including UTF-8, both endianness of UTF-16 and UTF-32, GB18030 and
  others) are tried but none of them successfully decoded the
  file. Did you accidentally upload an Excel file instead?  |]

errCSVHeaderNotFound headers = [stext| Cannot find the required table
  columns in the uploaded CSV file. The CSV file must contain the
  following required columns: #{headers}, preceded by a header row.  |]

errCSVCellNotFound i j = [stext| Cannot find the cell #{j}#{i} while
  processing the uploaded CSV file. Perhaps the CSV file is truncated
  or damaged during uploading.  |]

errCSVDuplicateColumnHeader cells = [stext| Cannot understand the
  table column headers because there seem to exist multiple columns
  with the same meaning. Please check the cells #{cells} of the
  uploaded CSV file. |]

errCSVClassNoParse row bs = [stext| Cannot understand “#{bs}” as a
  class in row #{row} of uploaded CSV file because it has incorrect
  format. The correct format is a number from 1 to 6, followed an
  uppercase letter except for O. |]

errCSVNricNoParse row nric = [stext| Cannot understand “#{nric}” as
  NRIC in row #{row} of the uploaded CSV file because it has incorrect
  format. The correct format of NRIC is: optionally “XXXX” followed by
  four decimal digits followed by an uppercase letter which must be
  one of “JZIHGFEDCBAXWUTRQPNMLK”. |]

errCSVSubjectCodeAmbiguous row subjcode interp1 interp2 = [stext|
  Cannot unambiguously understand the subject codes “#{subjcode}” in
  row #{row} of uploaded CSV file because there are multiple
  interpretations: it could mean either #{interp1} or #{interp2}. |]

errCSVSubjectCodeIncomplete row subjcode rem ps = [stext| Cannot
  completely understand the subject codes “#{subjcode}” in row
  #{row} of uploaded CSV file. A portion of the subject codes are
  understood: #{ps} but the remaining part “#{rem}” could not be
  understood. |]

errCSVSubjectCodeNothing row subjcode = [stext| Cannot understand the
  subject codes “#{subjcode}” in row #{row} of uploaded CSV
  file. Please check the valid subjects and the spelling of subject
  code. |]

errCSVSubjectCodeInternalError row subjcode = [stext| Cannot
  understand the subject codes “#{subjcode}” in row #{row} of
  uploaded CSV file due to an internal error. This should not
  happen. |]

errCSVIndexNumNoParse row bs = [stext| Cannot understand “#{bs}” as an
  integer for index number in row #{row} of uploaded CSV file. |]

errCSVWitnessNoParse row s = [stext| Cannot understand “#{s}” as a
  witness name in row #{row} of uploaded CSV file. Please check the
  spelling carefully. Use a hyphen if you intend to mean no witness.
  |]
