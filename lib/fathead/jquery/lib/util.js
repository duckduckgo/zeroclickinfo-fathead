'use strict'

// Groups an array of objects by the value of property on each object.
exports.groupBy =
  (property, objects) =>
    objects.reduce(
      (map, object) =>
        appendToKey(map, object[property], object),
      new Map()
    )

const appendToKey =
  (map, key, object) => {
    getWithDefault(map, key, []).push(object)

    return map
  }

const getWithDefault =
  (map, key, value) => {
    if (map.has(key)) {
      return map.get(key)
    }

    map.set(key, value)
    return value
  }

// Sorts an array of objects by the value of property on each object.
exports.sortBy =
  (property, objects) =>
    objects.sort(
      (a, b) =>
        cmp(a[property], b[property])
    )

const cmp =
  (a, b) =>
    a > b ? 1
    : a < b ? -1
    : 0