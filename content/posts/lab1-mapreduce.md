+++
title = "6.824 Lab 1 MapReduce"
date = 2022-07-18
lastmod = 2022-07-18T20:38:13+08:00
tags = ["distributed-system", 6.824]
draft = false
AUTHOR = "Stephanos Komnenos"
+++

## 各个结构体的定义 {#各个结构体的定义}


### Coordinator {#coordinator}

Coordinator 保存所有未完成的 map 和 reduce 任务，并且对整个结构体用锁确保不出现冲突读写。

```go
type Coordinator struct {
  // Your definitions here.

  MapTasks    map[int]TaskCandidate
  ReduceTasks map[int]TaskCandidate
  NReduce     int
  NMap        int

  mutex sync.Mutex
}

type TaskCandidate struct {
  FileName  string
  Status    int
  StartTime time.Time
}
```


### RPC {#rpc}

```go
const (
  Empty = iota
  Map
  Reduce
)

const (
  Idle = iota
  Running
  Success
  Failed
)

type Task struct {
  Id       int
  Type     int
  FileName string
  Status   int
  NReduce  int
  NMap     int
}
```

Go 居然没有 enum，导致很容易出现 `Task.Type = Idle` 或者 `Task.Status = Map` 这样不对应的情况，编辑器也没有办法提示。
（或者有什么别的办法？）


## Coordinator {#coordinator}

接收 RPC 参数的函数，其中 args 是 worker 的上一个任务，reply 是需要该函数指派的任务。

```go
func (c *Coordinator) AssignTask(args *Task, reply *Task) error
```

`args` 的处理规则：

-   如果上一个任务执行成功，则将其从待执行的任务表中（即 `MapTasks` 或 `ReduceTasks` ）移除；
-   否则将待执行任务表中的该任务标记为 `Idle` ，等待下一次分配。

`reply` 的分配规则：

-   所有 map 任务执行完毕后，再开始分配 reduce 任务；
-   从待执行任务表中寻找状态为 `Idle` 或开始时间早于10秒前的任务作为 `reply` ；
-   若无任务可分配，则设置 `reply.Type = Empty` 。


## Worker {#worker}

```go
func Worker(mapf func(string, string) []KeyValue,
  reducef func(string, []string) string) {

  lastTask := Task{Type: Empty}
  newTask := Task{Type: Empty}

  for {
    ok := call("Coordinator.AssignTask", &lastTask, &newTask)
    if !ok {
      fmt.Println("call failed!")
      newTask.Type = Empty
    }

    if newTask.Type == Empty {
      time.Sleep(time.Second)
    } else if newTask.Type == Map {
      doMapTask(mapf, &newTask)
    } else if newTask.Type == Reduce {
      doReduceTask(reducef, &newTask)
    }

    lastTask = newTask
    newTask = Task{Type: Empty}
  }
}
```

-   Map task 产生的中间结果保存在 `mr-intermediate-{$mapTaskId}-{0..$NReduce}` 文件中。
-   Reduce task 从 `mr-intermediate-{0..$NMap}-{$reduceTaskId}` 文件中载入数据，输出到 `mr-out-{$reduceTaskId}` 中。


## 未实现的部分 {#未实现的部分}

> To ensure that nobody observes partially written files in the presence of crashes,
> the MapReduce paper mentions the trick of using a temporary file and atomically renaming
> it once it is completely written. You can use `ioutil.TempFile` to create a temporary file
> and os.Rename to atomically rename it.

测试脚本看起来不包含这种情况，所以目前是全 PASS。