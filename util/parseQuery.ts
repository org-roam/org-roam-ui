import path from 'path'
import { OrgRoamNode } from '../api'
import { NodeObject } from 'force-graph'

export type Props = { [prop: string]: string }
export type Queries = { [query: string]: Query }
export type Query = {
  list: string
  tags: string[]
  titles: string[]
  files: string[]
  dirs: string[]
  props: { [prop: string]: string }
  mtimes: string[]
  ctimes: string[]
  todos: string[]
  queries: { [query: string]: Query }
}

const keywordList = ['title', 'tag', 'file', 'dir', 'prop', 'mtime', 'ctime', 'todo', 'query']

const getKeyWords = (query: string, keyword: string) => {
  const its = query.matchAll(new RegExp(`${keyword}:\\(([^\)]*?)\\)`, 'g'))
  const spread = Array.from(its)
  return spread.length ? spread.map((key) => key[1]) : []
}

export const emptyQuery: Query = {
  list: 'color',
  titles: [],
  files: [],
  tags: [],
  dirs: [],
  queries: {},
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
// }
export function parseQuery(queryString: string, queries: Queries) {
  return Object.keys(emptyQuery).reduce<Query>((acc, key) => {
    switch (key) {
      case 'queries':
        acc[key] = Object.fromEntries(getKeyWords(queryString, 'query').map((q) => [q, queries[q]]))
        return acc
      case 'list':
        return { ...acc, list: 'color' }
      case 'props':
        return { ...acc, props: {} }
      default:
        return { ...acc, [key]: getKeyWords(queryString, key.slice(0, -1)) }
    }
  }, emptyQuery)
}

export function filterNodeByQuery(node: OrgRoamNode, query: Query): boolean {
  const list = query.list
  return Object.entries(query).some((entry) => {
    const [keyword, value] = entry
    switch (keyword) {
      case 'titles':
        return node.title === value
      case 'files':
        return node.file === value
      case 'queries':
        return Object.values(query.queries)?.some((q: Query) => filterNodeByQuery(node, q))
      case 'dirs':
        return query.dirs?.some((dir) => path.dirname(node.file)?.includes(dir))
      case 'tags':
        return node.tags?.some((tag) => query?.tags?.includes(tag))
      case 'props':
        return Object.keys(query.props)?.some(
          (prop) => node.properties?.[prop] === query.props?.[prop],
        )
      case 'mtimes':
        return node.properties?.mtime === value
      case 'ctimes':
        return node.properties?.[keyword] === value
      default:
        return !list
    }
  })
}

export function filterNodesByQuery(nodes: NodeObject[], query: Query) {
  if (query.list === 'color') {
    return nodes
  }
  const list = query.list === 'white' ? true : false

  return nodes.filter((nodeArg) => {
    const node = nodeArg as OrgRoamNode
    return filterNodeByQuery(node, query) === list
  })
}
