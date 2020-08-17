(defun jme:mu4e-decode-mailer (mailer)
  "Produce an identifier for a MAILER user agent string."
  (cond
   ((string-prefix-p "Zimbra" x)
    (let ((clientstart (string-match "(" x)))
      (if clientstart
          (let* ((clientend (string-match "[ /]" (substring x (+ 1 clientstart))))
                 (clientstr (substring (substring x (+ 1 clientstart)) 0 clientend)))
            (cond
             ((string-prefix-p "Zimbra-ZCO" clientstr)
              (all-the-icons-fileicon "microsoft-infopath"))
             ((string-prefix-p "ZimbraWebClient" clientstr)
              (all-the-icons-alltheicon "html5"))
             (t
              (message (format "Mailer: unknown Zimbra Client: %s" x))
              (all-the-icons-faicon "weixin"))))
        (all-the-icons-faicon "weixin"))))
   ((string-prefix-p "Microsoft Outlook" x)
    (all-the-icons-faicon "windows"))
   ((string-prefix-p "Apple Mail" x)
    (all-the-icons-faicon "apple"))
   ((string-prefix-p "MailChimp" x)
    (all-the-icons-fileicon "monkey"))
   ((string-prefix-p "PHP" x)
    (all-the-icons-fileicon "php"))
   (t
    (if (> (length mailer) 1)
        (progn
          (message (format "Mailer: unknown client: %s" x))
          (all-the-icons-faicon "question-circle-o"))
      ""))))
