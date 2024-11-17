import * as $io from "../../gleam_stdlib/gleam/io.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $doc from "../glam/doc.mjs";
import { toList, CustomType as $CustomType } from "../gleam.mjs";

export class Task extends $CustomType {
  constructor(status, description, subtasks) {
    super();
    this.status = status;
    this.description = description;
    this.subtasks = subtasks;
  }
}

export class Todo extends $CustomType {}

export class Done extends $CustomType {}

export class InProgress extends $CustomType {}

function status_to_bullet(status) {
  if (status instanceof Todo) {
    return "- [ ]";
  } else if (status instanceof Done) {
    return "- [X]";
  } else {
    return "- […]";
  }
}

export function example_todo_list() {
  return toList([
    new Task(
      new InProgress(),
      "publish Glam v1.1.0",
      toList([
        new Task(new InProgress(), "write a tutorial on todo lists", toList([])),
        new Task(
          new InProgress(),
          "add `doc.flex_break`",
          toList([
            new Task(new Done(), "add the appropriate type variant", toList([])),
            new Task(new Done(), "implement the missing cases", toList([])),
            new Task(new Todo(), "add some tests", toList([])),
          ]),
        ),
      ]),
    ),
    new Task(new Todo(), "get some sleep", toList([])),
  ]);
}

function task_to_doc(task) {
  let task_line = (status_to_bullet(task.status) + " ") + task.description;
  let task_doc = $doc.from_string(task_line);
  let $ = $list.is_empty(task.subtasks);
  if ($) {
    return task_doc;
  } else {
    let _pipe = toList([task_doc, $doc.soft_break, tasks_to_doc(task.subtasks)]);
    let _pipe$1 = $doc.concat(_pipe);
    return $doc.nest(_pipe$1, 2);
  }
}

export function tasks_to_doc(tasks) {
  let _pipe = $list.map(tasks, task_to_doc);
  let _pipe$1 = $doc.join(_pipe, $doc.soft_break);
  return $doc.force_break(_pipe$1);
}

export function main() {
  let _pipe = example_todo_list();
  let _pipe$1 = tasks_to_doc(_pipe);
  let _pipe$2 = $doc.to_string(_pipe$1, 10_000);
  return $io.println(_pipe$2);
}
