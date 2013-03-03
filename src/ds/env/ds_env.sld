(define-library
 (ds env)
 (import "fmt" "reflect" "runtime/debug")
 (export
  environment-define
  environment-export
  environment-extend
  environment-import
  environment-ref
  environment-set!
  environment-update)
 (begin

  (go:type Env
    (go:struct
     #(it (go:slice go:internal:frame))))

  (go:func #(env (go:ptr Env)) Add
    (#(name go:string) #(value (go:interface)))
    go:void
    (go:= ((go:index (go:index env.it 0) (value.Name)))
          value))

  (go:func #(env (go:ptr Env)) AddProc
    (#(value (go:interface)) #(name go:string))
    go:void
    (go:= ((go:index env.it name))
          (NewProc value name)))

  (go:func #(env (go:ptr Env)) AddValue
    (#(value (go:interface)) #(name go:string))
    go:void
    (go:= ((go:index env.it name)) value))

  (go:func #(env (go:ptr Env)) Define
    (#(symbol value (go:interface)))
    go:void
    (env.defineString (go:as symbol go:string) value))

  (go:func
    #(env (go:ptr Env))
    defineString
    (#(name go:string) #(value (go:interface)))
    go:void
    (go:= ((go:index env.it name)) value))

  (go:func #(env (go:ptr Env)) Extend
    ()
    (go:ptr Env)
    (go:return (env.Update (NewEnv))))

  (go:func #(env (go:ptr Env)) Import
    (#(frame go:internal:frame))
    (go:ptr Env)
    (go::= child (NewEnv))
    (go:range
      (:= (key obj) frame)
      (go::= value (reflect.ValueOf obj))
      (go:case!
        (value.Kind)
        ((reflect.Func) (child.AddProc obj key))
        (go:else
          (fmt.Errorf
            "env.Import unrecognized type %v\n"
            obj))))
    (go:return (env.Update child)))

  (go:func #(env (go:ptr Env)) Ref
    (#(symbol (go:interface)))
    (go:interface)
    (go:return
      (env.refString (go:as symbol go:string))))

  (go:func #(env (go:ptr Env)) refString
    (#(name go:string))
    (go:interface)
    (go::= value (env.referString name go:nil))
    (go:when
      (go:== value go:nil)
      (debug.PrintStack)
      (go:panic (go:+ "unbound symbol " name)))
    (go:return value))
  (go:func
    #(env (go:ptr Env))
    Refer
    (#(symbol value (go:interface)))
    (go:interface)
    (go:return
      (env.referString (go:as symbol go:string) value)))
  (go:func
    #(env (go:ptr Env))
    referString
    (#(name go:string) #(value (go:interface)))
    (go:interface)
    (go:when
      (go:!= (go:index env.it name) go:nil)
      (go:return (go:index env.it name)))
    (go:when
      (go:!= env.parent go:nil)
      (go:return
        ((go:dot env.parent referString) name value)))
    (go:return value))
  (go:func #(env (go:ptr Env)) Set
    (#(symbol value (go:interface)))
    go:void
    (go::= name (go:as symbol go:string))
    (env.setString name value))
  (go:func #(env (go:ptr Env)) setString
    (#(name go:string) #(value (go:interface)))
    go:void
    (go:when
      (go:!= (go:index env.it name) go:nil)
      (go:= ((go:index env.it name)) value)
      (go:return))
    (go:when
      (go:!= env.parent go:nil)
      ((go:dot env.parent setString) name value)
      (go:return))
    (go:panic "set! expected bound variable"))

  (go:func #(env (go:ptr Env)) Update
    (#(child (go:ptr Env)))
    (go:ptr Env)
    (go:return
      (go:new:
        Env
        (go:: it child.it)
        (go:: parent env))))

  (go:func #(env (go:ptr Env)) AddEnv
    (#(child (go:ptr Env)))
    (go:ptr Env)
    (go:return
      (go:new:
        Env
        (go:: it child.it)
        (go:: parent env))))

  (go:func #(env (go:ptr Env)) AddEnvFrame
    (#(child go:internal:frame))
    (go:ptr Env)
    (go:return
      (go:new:
        Env
        (go:: it child.it)
        (go:: parent env))))

  (define (empty-environment)
    (go:new: Env (go:: it (go:make: (go:slice go:internal:frame)))))

  (define (environment . import-specs)
    (error "environment is only available at compile-time"))
  (define (environment-define env symbol value)
    (:= name (->immutable-string symbol))
    (as env (ptr Env) (Define name value))
    (void))
  (define (environment-extend env)
    (as env (ptr Env) (Extend)))
  (define (environment-ref env symbol (value nil))
    (:= name (->imm-string symbol))
    (as env (ptr Env) (Ref name value)))
  (define (environment-set! env symbol value)
    (:= name (->imm-string symbol))
    (as env (ptr Env) (Set name value))
    (void))
  (define (environment-update env)
    (as env (ptr Env) (Update)))
  (define (environment-import env frame)
    ((go:dot (go:as env go:env) Import) frame))
  (define (environment-export env)
    ((go:dot (go:as env go:env) Export)))

 )
)

