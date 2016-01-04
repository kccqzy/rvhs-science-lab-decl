import logging
import json
import base64
import webapp2
from RNCryptor import RNCryptor

from google.appengine.api import mail
from google.appengine.ext import blobstore
import cloudstorage as gcs

bucket_name = 'rvhs-lab-declaration-pdfs'

class Homepage(webapp2.RequestHandler):
    def get(self):
        self.response.set_status(422)

def decrypt(req):
            decoded = base64.b64decode(req)
            cryptor = RNCryptor()
            decrypted = cryptor.decrypt(decoded, 'TXyT-oqOs-Jwq0-knne-Vui8-1ABd-R1b5-56jL-zhph-LcwL-PzfZ-HIkv-4sLt-AHMK-3LO3-mXEj')
            obj = json.loads(decrypted)
            logging.info("Received object %s", obj)
            return obj

class MailDaemon(webapp2.RequestHandler):
    def post(self):
        try:
            req = self.request.get('payload')
            mail_obj = decrypt(req)
            mail_msg = mail.EmailMessage()
            for key in ('sender', 'subject', 'to', 'body'):
                setattr(mail_msg, key, mail_obj[key])
            attachments = [(name, base64.b64decode(content)) for name, content in mail_obj['attachments']]
            if attachments:
                mail_msg.attachments = attachments
        except Exception as e:
            self.response.set_status(400)
            return
        try:
            mail_msg.send()
        except Exception as e:
            logging.error('Send mail failed: %s', e)
            mail.send_mail_to_admins(sender='rvhs.science.oracle@gmail.com', subject='Attempt to send email rejected by Google',
                                     body='An attempt to send email to %s was rejected by Google.\n\nTechnical details:\n%s' % (mail_obj['to'], e))
        self.response.set_status(204)


class StorageDaemon(webapp2.RequestHandler):

    def post(self):
        try:
            req = self.request.get('payload')
            file_obj = decrypt(req)
            file_name = '/%s/%s' % (bucket_name, file_obj['name'])
            file_ct = file_obj['content_type']
            file_content = file_obj['content']
        except Exception as e:
            self.response.set_status(400)
        else:
            with gcs.open(file_name, 'w', content_type=file_ct) as gcsf:
                gcsf.write(base64.b64decode(file_content))
            self.response.set_status(204)

    def get(self):
        try:
            file_name = self.request.get('filename')
            assert file_name
            info = gcs.stat('/%s/%s' % (bucket_name, file_name))
            assert info is not None
            blob_key = blobstore.create_gs_key('/gs/%s/%s' % (bucket_name, file_name))
            assert blob_key is not None

        except Exception as e:
            self.response.set_status(400)
        else:
            self.response.headers['X-AppEngine-BlobKey'] = blob_key
            self.response.headers['Content-Type'] = info.content_type.encode()
            self.response.headers['Content-Disposition'] = ('attachment; filename="%s"' % file_name).encode()

app = webapp2.WSGIApplication([
    (r'/', Homepage),
    (r'/mail', MailDaemon),
    (r'/storage', StorageDaemon),
])
