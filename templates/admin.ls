;;; -*- mode: lisp; -*-
"use strict"

;; A few shortcuts to save typing. This is not real macros because it
;; performs replacement only, doesn't run code, i.e. effectively
;; everything is inside a quasiquote. And the syntax is weird. And
;; since it doesn't support quoting or symbol-name or stringification,
;; the displayName will be attached at runtime...
(macro def (rest...)
       (var ~rest...))
(macro fn (args rest...)
       (function ~args ~rest...))
(macro defn (name args rest...)
       (def ~name (fn ~args ~rest...)))
(macro obj (rest...)
       (object ~rest...))
(macro e (name attrs rest...)
       (React.createElement ~name (obj ~@attrs) ~rest...))
(macro defcomponent (name rest...)
       (def ~name (React.createClass (_.defaults (obj ~rest...) (obj render (fn () false)) (_.invert (obj ~name "displayName"))))))

($
 (fn ()
   (defn APIConnection (pathname)
     (def wsUrl
          (+ (+ (if (= window.location.protocol "https:") "wss://" "ws://")
                window.location.host)
             pathname)
          connAttempts 0
          conn null
          callback null)
     (defn retry ()
       (when (< ++connAttempts 10)
         (set conn (new WebSocket wsUrl))
         (set conn.onmessage callback)
         (set conn.onerror (fn ()
                             (console.log "WS connection errored; how?")
                             (retry)))
         (set conn.onclose (fn ()
                             (console.log "WS connection closed; why?")))))
     (retry)
     (obj
       registerCallback (fn (func)
                          (set conn.onmessage func)
                          (set callback func))
       readyState (fn ()
                    (if conn conn.readyState -1))
       close (fn () (conn.close))))

   (defcomponent EntityRow
     render
     (fn ()
       (def categoryTh
              (if this.props.firstRowSpan
                (e "th" (rowSpan this.props.firstRowSpan) this.props.entity.category)
                ""))
       (e "tr" ()
         categoryTh
         (e "td" () this.props.entity.name)
         (e "td" (className "text-right")
           (e "div" (className "btn-group" role "group" "aria-label" "Action Buttons")
             (e "button" (type "button" className "btn btn-default btn-xs" "aria-label" "Edit")
               (e "span" (className "glyphicon glyphicon-pencil" "aria-hidden" "true")))
             (e "button" (type "button" className "btn btn-default btn-xs" "aria-label" "Delete")
               (e "span" (className "glyphicon glyphicon-trash" "aria-hidden" "true"))))))
       ))

   (defcomponent EntityGroup
     render
     (fn ()
       (e "tbody" ()
         (_.map this.props.entities
                (fn (entity i entities)
                  (e EntityRow
                      (key entity.id entity entity firstRowSpan (if i 0 entities.length))))))))

   (defcomponent EntityTable
     getInitialState
     (fn () (obj entities (obj)))
     componentDidMount
     (fn ()
       (def that this)
       (this.props.conn.registerCallback
        (fn (e)
          (def massagedData (_.map (_.sortBy (_.map (_.groupBy (.data (JSON.parse e.data)) "category")
                                                    (fn (v k) (obj k k v (_.sortBy v "name"))))
                                             "k")
                                   (fn (d) (.v d))))
          (that.setState (obj entities massagedData)))))
     componentWillUnmount
     (fn () (this.props.conn.close))
     render
     (fn ()
       (def rows (_.map this.state.entities
                        (fn (entities idx)
                          (e EntityGroup
                              (entities entities key idx)))))
       (e "div" (className "table-responsive")
         (e "table" (className "table")
           (e "thead" ()
             (e "tr" ()
               (e "th" () "Category")
               (e "th" () "Name")
               (e "th" () "")))
           rows))))

   (defcomponent Modal
     render
     (fn ()
       (def header
            (e "div" (className "modal-header")
              (if this.props.canClose
                (e "button" (type "button" className "close" "data-dismiss" "modal")
                  (e "span" ("aria-hidden" "true") "×")
                  (e "span" (className "sr-only") "Close"))
                "")
              (e "h4" (className "modal-title") this.props.title)))
       (def footer
            (if this.props.buttons
              (e "div" (className "modal-footer") this.props.buttons)
              ""))
       (e "div" (id "modal" className "modal fade")
         (e "div" (className "modal-dialog")
           (e "div" (className "modal-content")
             header
             (e "div" (className "modal-body") this.props.children)
             footer))))
     componentDidMount
     (fn ()
       (-> ($ (this.getDOMNode))
           (.modal (obj keyboard false backdrop "static")))))

   (defcomponent AdminHomeR
     render
     (fn ()
       (e "div" (className "row")
         (e "div" (className "col-sm-11 col-md-8 col-lg-7")
           (e "h2" () "Welcome")
           (e "p" () "Welcome to the admin console for RVHS Science Lab Undertaking Project. XXX Be verbose.")
           (e "h2" () "Quick Guide")
           (e "p" () "TODO")
           (e "h2" () "API Documentation")
           (e "p" () "TODO")))))

   (defcomponent EntityView
     render
     (fn ()
       (def name this.props.name)
       (defn willShow ()
         (React.render (e EntityTable (conn (APIConnection (+ (+ "/api/" name) "s")))) (-> ($ (+ "#" name)) (.get 0))))
       (defn didHide ()
         (React.unmountComponentAtNode (-> ($ (+ "#" name)) (.get 0))))
       (e BSTab (active 0 target (+ "#" name) label this.props.label willShow willShow didHide didHide))))

   (defcomponent AdminCcasR
     render
     (fn ()
       (e "div" ()
         (e "div" (className "pull-right btn-group" role "toolbar" "aria-label" "Action Buttons")
           (e "button" (type "button" className "btn btn-default") "Add New")
           (e "button" (type "button" className "btn btn-default") "Remove All"))
         (e "h2" () "All CCAs")
         (e EntityTable (conn (APIConnection "/api/ccas"))))))

   (defcomponent AdminSubjectsR)

   (defcomponent AdminTeachersR)

   (defcomponent AdminStudentsR)

   (def routes (obj "/admin" (array "Home" AdminHomeR)
                    "/admin/ccas" (array "Manage CCAs" AdminCcasR)
                    "/admin/subjects" (array "Manage Subjects" AdminSubjectsR)
                    "/admin/teachers" (array "Manage Teachers" AdminTeachersR)
                    "/admin/students" (array "Manage Students" AdminStudentsR)))

   (defcomponent Page
     render
     (fn ()
       (def pathname window.location.pathname)
       (def tabs (_.map routes (fn (tuple route)
                                 (e "li" (key route role "presentation" className (if (= route pathname) "active" ""))
                                   (e "a" (href (if (= route pathname) "#" route))
                                     (get 0 tuple))))))

       (e "div" (id "content-wrapper")
         (e "div" (id "modal-wrapper"))
         (e "div" (className "container")
           (e "div" (className "page-header")
             (e "h1" () "RVHS Science Lab Undertaking — For Teachers and Administrators"))
           (e "p" () "You are logged in as xxx.")
           (e "div" (role "tabpanel")
             (e "ul" (className "nav nav-tabs")
               tabs))
           (e "div" (id "main-content")))))
     componentDidMount
     (fn ()
       (def pathname window.location.pathname)
       (if (undefined? window.WebSocket)
         (React.render
          (e Modal (canClose 0 title "Browser Unsupported")
            (e "p" ()
              "Your browser is too old to use this website. This website requires at least Internet Explorer version 10, Apple Safari version 7, Google Chrome version 16, or Mozilla Firefox version 11. Regardless of which broswer you are using, it is always recommended that you use the latest version available."))
          (-> ($ "#modal-wrapper") (.get 0)))
         (React.render
          (e (get 1 (get pathname routes)) ())
          (-> ($ "#main-content") (.get 0))))))

   (React.render (e Page ()) document.body)))


;;; Local variables:
;;; enable-local-eval: t
;;; eval: (put 'fn 'lisp-indent-function 'defun)
;;; eval: (put 'e 'lisp-indent-function 2)
;;; eval: (put 'if 'lisp-indent-function 1)
;;; End:
