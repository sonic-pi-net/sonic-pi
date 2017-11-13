#lang racket

;; Single-line comment

#| Multi-line comment on one line |#

#| Multi-line comment on
   two lines |#

'symbol
`symbol
'(a quoted list)
`(a quasiquoted expr with ,unquasiquoted item)
#:keyword
-inf.f
+inf.f
-inf.0
+inf.0
-min.0
+max.0
-nan.0
+nan.0

(define (1-crazy-identifier-疯狂的标识符-τρελό-αναγνωριστικό x)
  (add1 x))
(check-equal? (1-crazy-identifier-疯狂的标识符-τρελό-αναγνωριστικό 1) 2)

(require xml net/url
         racket/control) ;; <<< new import

(define (serve port-no)
  (define main-cust (make-custodian))
  (parameterize ([current-custodian main-cust])
    (define listener (tcp-listen port-no 5 #t))
    (define (loop)
      (accept-and-handle listener)
      (loop))
    (thread loop))
  (λ ()
    (custodian-shutdown-all main-cust)))

(define (accept-and-handle listener)
  (define cust (make-custodian))
  (custodian-limit-memory cust (* 50 1024 1024))
  (parameterize ([current-custodian cust])
    (define-values (in out) (tcp-accept listener))
    (thread (λ ()
              (handle in out)
              (close-input-port in)
              (close-output-port out))))
  ;; Watcher thread:
  (thread (λ ()
            (sleep 10)
            (custodian-shutdown-all cust))))

(define (handle in out)
  (define req
    ;; Match the first line to extract the request:
    (regexp-match #rx"^GET (.+) HTTP/[0-9]+\\.[0-9]+"
                  (read-line in)))
  (when req
    ;; Discard the rest of the header (up to blank line):
    (regexp-match #rx"(\r\n|^)\r\n" in)
    ;; Dispatch:
    (let ([xexpr (prompt (dispatch (list-ref req 1)))]) ;; <<< changed
      ;; Send reply:
      (display "HTTP/1.0 200 Okay\r\n" out)
      (display "Server: k\r\nContent-Type: text/html\r\n\r\n" out)
      (display (xexpr->string xexpr) out))))

(define (dispatch str-path)
  ;; Parse the request as a URL:
  (define url (string->url str-path))
  ;; Extract the path part:
  (define path (map path/param-path (url-path url)))
  ;; Find a handler based on the path's first element:
  (define h (hash-ref dispatch-table (car path) #f))
  (if h
      ;; Call a handler:
      (h (url-query url))
      ;; No handler found:
      `(html (head (title "Error"))
             (body
              (font ((color "red"))
                    "Unknown page: "
                    ,str-path)))))

(define dispatch-table (make-hash))

(hash-set! dispatch-table "hello"
           (λ (query)
             `(html (body "Hello, World!"))))

;; ----------------------------------------

(define (build-request-page label next-url hidden)
  `(html
    (head (title "Enter a Number to Add"))
    (body ([bgcolor "white"])
          (form ([action ,next-url] [method "get"])
                ,label
                (input ([type "text"] [name "number"]
                        [value ""]))
                (input ([type "hidden"] [name "hidden"]
                        [value ,hidden]))
                (input ([type "submit"] [name "enter"]
                        [value "Enter"]))))))

(define (many query)
  ;; Create a page containing the form:
  (build-request-page "Number of greetings:" "/reply" ""))

(define (reply query)
  ;; Extract and use the form results:
  (define n (string->number (cdr (assq 'number query))))
  `(html (body ,@(for/list ([i (in-range n)])
                   " hello"))))

(hash-set! dispatch-table "many" many)
(hash-set! dispatch-table "reply" reply)

;; ----------------------------------------
;; Old, awkward version:

(define (sum query)
  (build-request-page "First number:" "/one" ""))

(define (one query)
  (build-request-page "Second number:"
                      "/two"
                      (cdr (assq 'number query))))

(define (two query)
  (let ([n (string->number (cdr (assq 'hidden query)))]
        [m (string->number (cdr (assq 'number query)))])
    `(html (body "The sum is " ,(number->string (+ m n))))))

(hash-set! dispatch-table "sum" sum)
(hash-set! dispatch-table "one" one)
(hash-set! dispatch-table "two" two)

;; ----------------------------------------

;; Helper to grab a computation and generate a handler for it:

(define (send/suspend mk-page)
  (let/cc k
    (define tag (format "k~a" (current-inexact-milliseconds)))
    (hash-set! dispatch-table tag k)
    (abort (mk-page (string-append "/" tag)))))

;; Helper to run the number-getting page via `send/suspend':

(define (get-number label)
  (define query
    ;; Generate a URL for the current computation:
    (send/suspend
     ;; Receive the computation-as-URL here:
     (λ (k-url)
       ;; Generate the query-page result for this connection.
       ;; Send the query result to the saved-computation URL:
       (build-request-page label k-url ""))))
  ;; We arrive here later, in a new connection
  (string->number (cdr (assq 'number query))))

;; ----------------------------------------

;; New direct-style servlet:

(define (sum2 query)
  (define m (get-number "First number:"))
  (define n (get-number "Second number:"))
  `(html (body "The sum is " ,(number->string (+ m n)))))

(hash-set! dispatch-table "sum2" sum2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang web-server/insta

(require web-server/formlets
         "model-3.rkt")

; start: request -> doesn't return
; Consumes a request and produces a page that displays
; all of the web content.
(define (start request)
  (render-blog-page
   (initialize-blog!
    (build-path (current-directory)
                "the-blog-data.sqlite"))
   request))

; new-post-formlet : formlet (values string? string?)
; A formlet for requesting a title and body of a post
(define new-post-formlet
  (formlet
   (#%# ,{input-string . => . title}
        ,{input-string . => . body})
   (values title body)))

; render-blog-page: blog request -> doesn't return
; Produces an HTML page of the content of the
; blog.
(define (render-blog-page a-blog request)
  (local [(define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "My Blog"))
                    (body
                     (h1 "My Blog")
                     ,(render-posts a-blog embed/url)
                     (form ([action
                             ,(embed/url insert-post-handler)])
                           ,@(formlet-display new-post-formlet)
                           (input ([type "submit"])))))))

          (define (insert-post-handler request)
            (define-values (title body)
              (formlet-process new-post-formlet request))
            (blog-insert-post! a-blog title body)
            (render-blog-page a-blog (redirect/get)))]

    (send/suspend/dispatch response-generator)))

; new-comment-formlet : formlet string
; A formlet for requesting a comment
(define new-comment-formlet
  input-string)

; render-post-detail-page: post request -> doesn't return
; Consumes a post and produces a detail page of the post.
; The user will be able to either insert new comments
; or go back to render-blog-page.
(define (render-post-detail-page a-blog a-post request)
  (local [(define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Post Details"))
                    (body
                     (h1 "Post Details")
                     (h2 ,(post-title a-post))
                     (p ,(post-body a-post))
                     ,(render-as-itemized-list
                       (post-comments a-post))
                     (form ([action
                             ,(embed/url insert-comment-handler)])
                           ,@(formlet-display new-comment-formlet)
                           (input ([type "submit"])))
                     (a ([href ,(embed/url back-handler)])
                        "Back to the blog")))))

          (define (insert-comment-handler request)
            (render-confirm-add-comment-page
             a-blog
             (formlet-process new-comment-formlet request)
             a-post
             request))

          (define (back-handler request)
            (render-blog-page a-blog request))]

    (send/suspend/dispatch response-generator)))

; render-confirm-add-comment-page :
; blog comment post request -> doesn't return
; Consumes a comment that we intend to add to a post, as well
; as the request. If the user follows through, adds a comment
; and goes back to the display page. Otherwise, goes back to
; the detail page of the post.
(define (render-confirm-add-comment-page a-blog a-comment
                                         a-post request)
  (local [(define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Add a Comment"))
                    (body
                     (h1 "Add a Comment")
                     "The comment: " (div (p ,a-comment))
                     "will be added to "
                     (div ,(post-title a-post))

                     (p (a ([href ,(embed/url yes-handler)])
                           "Yes, add the comment."))
                     (p (a ([href ,(embed/url cancel-handler)])
                           "No, I changed my mind!"))))))

          (define (yes-handler request)
            (post-insert-comment! a-blog a-post a-comment)
            (render-post-detail-page a-blog a-post (redirect/get)))

          (define (cancel-handler request)
            (render-post-detail-page a-blog a-post request))]

    (send/suspend/dispatch response-generator)))

; render-post: post (handler -> string) -> xexpr
; Consumes a post, produces an xexpr fragment of the post.
; The fragment contains a link to show a detailed view of the post.
(define (render-post a-blog a-post embed/url)
  (local [(define (view-post-handler request)
            (render-post-detail-page a-blog a-post request))]
    `(div ([class "post"])
          (a ([href ,(embed/url view-post-handler)])
             ,(post-title a-post))
          (p ,(post-body a-post))
          (div ,(number->string (length (post-comments a-post)))
               " comment(s)"))))

; render-posts: blog (handler -> string) -> xexpr
; Consumes a embed/url, produces an xexpr fragment
; of all its posts.
(define (render-posts a-blog embed/url)
  (local [(define (render-post/embed/url a-post)
            (render-post a-blog a-post embed/url))]
    `(div ([class "posts"])
          ,@(map render-post/embed/url (blog-posts a-blog)))))

; render-as-itemized-list: (listof xexpr) -> xexpr
; Consumes a list of items, and produces a rendering as
; an unorderered list.
(define (render-as-itemized-list fragments)
  `(ul ,@(map render-as-item fragments)))

; render-as-item: xexpr -> xexpr
; Consumes an xexpr, and produces a rendering
; as a list item.
(define (render-as-item a-fragment)
  `(li ,a-fragment))
