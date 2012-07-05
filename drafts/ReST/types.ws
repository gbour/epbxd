
sample:
  record: sample
  key   : field1

  fields:
    field1:
      type     : integer
      default  : 1
      desc     : "plop plop"
      required : true
      unique   : true
      #constraint: xxx
    field5:
      type     : boolean
      default  : true
    field6: &ref1
      type     : string
      default  : "foo"
    field7:
      type     : list(integer)
      #default : []
    field8:
      type     : dict(string)
      required : true
      default  :
        key: "val"
        foo: "bar"
      #plop: *ref1
      
user:
  record : rc_user
  key    : uid

  fields :
    uid:
      type     : string
      desc     : "user unique identifier"
    name:
      type     : string
      required : true
      unique   : true
      desc     : "user name"
    groups:
      type     : list(group)
      default  : []

group:
  record : rc_group
  key    : gid

  fields : 
    gid:
      type     : integer
      desc     : "group unique identifier"
    name:
      type     : string
      required : true
      unique   : true
      desc     : "group name"
    owner:
      type     : user
      required : true
      desc     : "group owner"
    users:
      type     : list(user)
      default  : []
      desc     : "group members"
    created: 
      type     : datetime
      default  : now
      desc     : "group creation date"
    paths:
      type     : list(string)
      default  : ['/bin', '/usr/bin']
      desc     : "binaries default paths"


      
   
