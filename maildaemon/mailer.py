import webapp2
import logging
import json
import base64
from RNCryptor import RNCryptor

from google.appengine.api import mail

class MailDaemon(webapp2.RequestHandler):
    def post(self):
        try:
            req = self.request.get('payload')
            decoded = base64.b64decode(req)
            cryptor = RNCryptor()
            decrypted = cryptor.decrypt(decoded, 'TXyT-oqOs-Jwq0-knne-Vui8-1ABd-R1b5-56jL-zhph-LcwL-PzfZ-HIkv-4sLt-AHMK-3LO3-mXEj')
            mail_obj = json.loads(decrypted)
            logging.info("Received mail object %s", mail_obj)
            mail_msg = mail.EmailMessage()
            for key in ('sender', 'subject', 'to', 'body'):
                setattr(mail_msg, key, mail_obj[key])
            attachments = [(name, base64.b64decode(content)) for name, content in mail_obj['attachments']]
            if attachments:
                mail_msg.attachments = attachments
        except Exception as e:
            self.response.set_status(400)
        else:
            logging.info('About to send email %s', mail_msg)
            mail_msg.send()
            self.response.set_status(204)

app = webapp2.WSGIApplication([
    (r'/', MailDaemon),
])
