import path from 'path'
import { OrgRoamNode } from '../api'
import { NodeObject } from 'force-graph'

export type Props = { [prop: string]: string[] }
export type Queries = { [name: string]: { mode: string; query: Query } }
export interface Query {
  tags: string[]
  titles: string[]
  files: string[]
  dirs: string[]
  props?: Props
  mtimes: string[]
  ctimes: string[]
  todos: string[]
  //queries: { [query: string]: Query }
}
//export type Query = {[key:string]: string |string[] | Props}

const keywordList = ['title', 'tag', 'file', 'dir', 'prop', 'mtime', 'ctime', 'todo', 'query']

const getKeyWords = (query: string, keyword: string) => {
  const its = query.matchAll(new RegExp(`${keyword}:\\(([^\)]*?)\\)`, 'g'))
  const spread = Array.from(its)
  return spread.length ? spread.map((key) => key[1]) : []
}

export const emptyQuery: Query = {
  titles: [],
  files: [],
  tags: [],
  dirs: [],
  // queries: {},
  ctimes: [],
  mtimes: [],
  props: {},
  todos: [],
}

// export function parseProps(queryString: string) {
//   return {
//     props: Object.fromEntries(
//       getKeyWords(queryString, 'prop').map((p) => {
//         const match = p.matchAll(/([^:]*?:(.*?)/g)
//         const arr = Array.from(match)
//         if (!arr?.length) return

//         if (!(arr[0].length > 2)) return
//         const [string, key, val] = arr[0]
//         return [key, val]
//       }),
//     ),
//   }
//
// }
//

export function mergeQueries(mainQuery: Query, mergeQuery: Query) {
  return Object.entries(mergeQuery).reduce<Query>((acc, entry: any, index: number): Query => {
    const [key, val] = entry
    switch (key) {
      case 'list':
        return acc
      case 'queries':
        return acc
      case 'props':
        return acc
      default:
        if (!Object.keys(acc).includes(key)) {
          return acc
        }
        return {
          ...acc,
          [key]: Array.from(new Set([...(Object.values(acc)[index] ?? []), ...val])),
        }
    }
  }, mainQuery)
}

export function parseSubQuery(queryString: string, acc: Query, queries: Queries) {
  const subQueryKeywords = getKeyWords(queryString, 'query')
  if (subQueryKeywords.length === 0) return
  const queryObj = subQueryKeywords.reduce<Query>((reduce, queryName: string) => {
    return mergeQueries(reduce, queries[queryName]?.query)
  }, acc)
  return queryObj
}
export function parseQuery(queryString: string, queries: Queries) {
  return Object.keys(emptyQuery).reduce<Query>((acc, key): Query => {
    switch (key) {
      case 'queries':
        return parseSubQuery(queryString, acc, queries) ?? acc
      case 'props':
        return { ...acc, props: {} }
      default:
        return { ...acc, [key]: getKeyWords(queryString, key.slice(0, -1)) }
    }
  }, emptyQuery)
}
export function filterPropIncludes(nodeProp: string[], queryProp: string[]) {
  return queryProp.some((p: string) => nodeProp.some((pr) => pr.includes(p)))
}

export function filterPropEqualsOne(nodeProp: string[], queryProp: string[]) {
  return queryProp.some((p: string) => nodeProp.includes(p))
}

export function filterNodeByQuery(node: OrgRoamNode, query: Query, list: boolean): boolean {
  return (
    !list ===
    Object.entries(query).some((entry) => {
      const [keyword, value] = entry
      if (keyword !== 'props' && value.length === 0) {
        return false
      }
      switch (keyword) {
        case 'titles':
          return filterPropIncludes([node.title], value)
        case 'files':
          return filterPropIncludes([node.file], value)
        case 'dirs':
          return value.some((dir: string) => path.dirname(node.file)?.includes(dir))
        case 'tags':
          return filterPropEqualsOne(node.tags, value)
          return node.tags?.some((tag) => query?.tags?.includes(tag))
        case 'props':
          return (
            Object.entries(query?.['props']!)?.some((prop) => {
              const [key, val] = prop
              return val.some((v) => node.properties?.[key] === v)
            }) ?? false
          )
        case 'mtimes':
          return node.properties?.mtime === value
        case 'ctimes':
          return node.properties?.ctime === value
        default:
          return false
      }
    })
  )
}

export function filterNodesByQuery(nodes: NodeObject[], query: Query, mode: string) {
  if (mode === 'color') {
    return nodes
  }
  const list = mode === 'white' ? true : false

  return nodes.filter((nodeArg) => {
    const node = nodeArg as OrgRoamNode
    return filterNodeByQuery(node, query, list) === list
  })
}

export function filterNodes(nodes: NodeObject[], queries: Queries): NodeObject[] {
  return Object.entries(queries).reduce<NodeObject[]>((acc, entry) => {
    const [name, { query, mode }] = entry
    return filterNodesByQuery(acc, query, mode)
  }, nodes)
}
