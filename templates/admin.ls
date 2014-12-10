;;; -*- mode: lisp; -*-
"use strict"

;; A few shortcuts to save typing.
(macro fn (args rest...)
       (function ~args ~rest...))
(macro defn (name args rest...)
       (var ~name (fn ~args ~rest...)))
(macro obj (rest...)
       (object ~rest...))
(macro e (name attrs rest...)
       (React.createElement ~name (obj ~@attrs) ~rest...))
(macro defcomponent (name rest...)
       (var ~name (React.createClass (_.defaults (obj ~rest...) (_.invert (obj ~name "displayName"))))))

($
 (fn ()
   (var APIConnection
        (fn (pathname)
          (var wsUrl
               (+ (+ (if (= window.location.protocol "https:") "wss://" "ws://")
                     window.location.host)
                  pathname)
               connAttempts 0
               conn null
               callback null)
          (defn retry ()
            (set connAttempts (+ 1 connAttempts))
            (when (< connAttempts 10)
              (set conn (new (WebSocket wsUrl)))
              (set conn.onmessage callback)
              (set conn.onerror (fn ()
                                  (console.log "WS connection errored")
                                  (retry)))
              (set conn.onclose (fn ()
                                  (console.log "WS connection closed")))))
          (retry)
          (obj
            registerCallback (fn (func)
                               (set conn.onmessage func)
                               (set callback func))
            readyState (fn ()
                         (if conn conn.readyState -1))
            close (fn () (conn.close)))))

   (defcomponent EntityRow
     render
     (fn ()
       (var categoryTh
            (if this.props.firstRowSpan
              (e "th" (rowSpan this.props.firstRowSpan) this.props.entity.category)
              ""))
       (e "tr" ()
         categoryTh
         (e "td" () this.props.entity.name))
       ))

   (defcomponent EntityGroup
     render
     (fn ()
       (e "tbody" ()
         (_.map this.props.entities
                (fn (entity i)
                  (e EntityRow
                      (key entity.id entity entity firstRowSpan (if i 0 this.props.entity.length))))))))

   (defcomponent EntityTable
     getInitialState
     (fn () (obj entities (obj)))
     componentDidMount
     (fn ()
       (var that this)
       (this.props.conn.registerCallback
        (fn (e)
          (var groupedData (_.groupBy (.data (JSON.parse e.data)) "category"))
          (var groupedDataSorted (_.object (_.map groupedData
                                                  (fn (v k)
                                                    (array k (_.sortBy v "name"))))))
          (that.setState (obj entities groupedDataSorted)))))
     componentWillUnmount
     (fn () (this.props.conn.close))
     render
     (fn ()
       (console.log "EntityTable render")
       (var rows (_.map this.state.entities
                        (fn (entities idx)
                          (e EntityGroup
                              (entities entities key idx)))))
       (e "div" (className "table-responsive")
         (e "table" (className "table")
           (e "thead" ()
             (e "tr" ()
               (e "th" () "Category")
               (e "th" () "Name")))
           rows))))

   (defcomponent BSTab
     render
     (fn ()
       (e "li" (role "presentation" className (if this.props.active "active" ""))
         (e "a" ("data-target" this.props.target "aria-controls" "home" role "tab" "data-toggle" "tab")
           this.props.label)))
     componentDidMount
     (fn ()
       (if this.props.willShow
         (-> ($ (this.getDOMNode))
             (.find "a")
             (.on "show.bs.tab" this.props.WillShow)))
       (if this.props.didHide
         (-> ($ (this.getDOMNode))
             (.find "a")
             (.on "hidden.bs.tab" this.props.didHide)))))

   (defcomponent BSTabContent
     render
     (fn ()
       (e "div" (role "tabpanel" className (if this.props.active "tab-pane fade in active" "tab-pane fade in") id this.props.name)
         this.props.children)))

   (defcomponent BSModal
     render
     (fn ()
       (var header
            (e "div" (className "modal-header")
              (if this.props.canClose
                (e "button" (type "button" className "close" "data-dismiss" "modal")
                  (e "span" ("aria-hidden" "true") "×")
                  (e "span" (className "sr-only") "Close"))
                "")
              (e "h4" (className "modal-title") this.props.title)))
       (var footer
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

   (defcomponent EntityTableTab
     render
     (fn ()
       (var name this.props.name)
       (defn willShow ()
         (React.render (e EntityTable (conn (APIConnection (+ (+ "/api/" name) "s")))) (-> ($ (+ "#" name)) (.get 0))))
       (defn didHide ()
         (React.unmountComponentAtNode (-> ($ (+ "#" name)) (.get 0))))
       (e BSTab (active 0 target (+ "#" name) label this.props.label willShow willShow didHide didHide))))

   (defcomponent Page
     render
     (fn ()
       (e "div" (id "modal-wrapper"))
       (e "div" (id "wrapper")
         (e "div" (className "container")
           (e "div" (className "page-header")
             (e "h1" () "RVHS Science Lab Undertaking — For Teachers and Administrators"))
           (e "p" () "You are logged in as xxx.")
           (e "div" (role "tabpanel")
             (e "ul" (className "nav nav-tabs" role "tablist")
               (e BSTab (active 1 target "#home" label "Home"))
               (e EntityTableTab (name "cca" label "Manage CCAs")))
             (e "div" (className "tab-content")
               (e BSTabContent (active 1 name "home")
                 (e "p" () "ehh"))
               (e BSTabContent (name "cca")))))))
     componentDidMount
     (fn ()
       (if (undefined? window.WebSocket)
         (React.render
          (e BSModal (canClose 0 title "Browser Unsupported")
            (e "p" ()
              "Your browser is too old to use this website. This website requires at least Internet Explorer version 10, Apple Safari version 7, Google Chrome version 16, or Mozilla Firefox version 11. Regardless of which broswer you are using, it is always recommended that you use the latest version available."))
          (-> ($ "#modal-wrapper") (.get 0))))))

   (React.render (e Page ()) document.body)))


;;; Local variables:
;;; enable-local-eval: t
;;; eval: (put 'fn 'lisp-indent-function 'defun)
;;; eval: (put 'e 'lisp-indent-function 2)
;;; eval: (put 'if 'lisp-indent-function 1)
;;; End:
