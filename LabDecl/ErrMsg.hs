{-*- mode: text; -*-}
{-# LANGUAGE QuasiQuotes #-}
module LabDecl.ErrMsg where

import Control.Lens
import qualified Data.Text as T
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
