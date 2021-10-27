import path from 'path'
import { OrgRoamNode } from '../api'
import { NodeObject } from 'force-graph'

export type Props = { [prop: string]: string[] }
export type Queries = { [name: string]: { mode: string; query: Query; color: string } }
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
export type LinQuery = { keyword: string; value: string[] }[]
export type LinQueries = { [name: string]: { mode: string; query: LinQuery } }
//export type Query = {[key:string]: string |string[] | Props}

const keywordList = ['title', 'tag', 'file', 'dir', 'prop', 'mtime', 'ctime', 'todo', 'query']

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

const getRegexMatches = (
  queryString: string,
  regex: string | RegExp,
  positions: number[] = [1],
  flags?: string,
) => {
  const reg = new RegExp(regex, flags || 'g')
  const its = queryString.matchAll(reg)
  const spread = Array.from(its)
  if (spread?.length === 0) return []
  return spread.flatMap((key) => positions.map((pos) => key[pos]))
}

const getKeyWords = (queryString: string, keyword: string) => {
  const keywordRegex = `${keyword}:\\(([^\)]*?)\\)`
  if (keyword !== 'title') {
    return getRegexMatches(queryString, keywordRegex)
  }

  const titleRegex = /(?<!\w+:\()\b(?!\w+:\()([^: \b]+)\b/
  return [
    ...getRegexMatches(queryString, keywordRegex),
    ...getRegexMatches(queryString.replaceAll(/\(.*\)|\w+:/g, ''), titleRegex),
  ]
}

// export function mergeQueries(mainQuery: LinQuery, mergeQuery: LinQuery) {
//   return Object.entries(mergeQuery).reduce<Query>((acc, entry: any, index: number): Query => {
//     const [key, val] = entry
//     switch (key) {
//       case 'list':
//         return acc
//       case 'queries':
//         return acc
//       case 'props':
//         return acc
//       default:
//         if (!Object.keys(acc).includes(key)) {
//           return acc
//         }
//         return {
//           ...acc,
//           [key]: Array.from(new Set([...(Object.values(acc)[index] ?? []), ...val])),
//         }
//     }
//   }, mainQuery)
// }

// export function parseSubQuery(queryString: string, acc: Query, queries: Queries) {
//   const subQueryKeywords = getKeyWords(queryString, 'query')
//   if (subQueryKeywords?.length === 0) return
//   const queryObj = subQueryKeywords.reduce<Query>((reduce, queryName: string) => {
//     return mergeQueries(reduce, queries[queryName]?.query)
//   }, acc)
//   return queryObj
// }

export function parseQuery(queryString: string, queries: LinQueries) {
  const keywordRegex = `([a-z]*):\\(([^\)]*?)\\)`
  const titleRegex = `(?<!\\w+:\\()\\b(?!\w+:\\()([^: \\b]+)\\b`
  const matches = Array.from(
    queryString.matchAll(new RegExp(`(${keywordRegex}|${titleRegex})`, 'g')),
  )
  const linq = matches.reduce<LinQuery>((acc, q) => {
    if (!q[2]) {
      return [...acc, { keyword: 'title', value: q[1].split(',').map((s) => s.trim()) }]
    }
    // if (q[2] === 'query') {
    //   if (queries[q[3]]) {
    //     console.log(q[3])
    //     return [...acc, ...queries[q[3]].query]
    //   }
    //   return acc
    // }
    return [...acc, { keyword: q[2], value: q[3].split(',').map((s) => s.trim()) }]
  }, [])

  return linq
}

// export function parseQueryStupidly(queryString: string, queries: Queries) {
//   return [...Object.keys(emptyQuery), 'queries'].reduce<Query>((acc, key): Query => {
//     switch (key) {
//       case 'queries':
//         return parseSubQuery(queryString, acc, queries) ?? acc
//       case 'props':
//         return { ...acc, props: {} }
//       default:
//         return { ...acc, [key]: getKeyWords(queryString, key.slice(0, -1)) }
//     }
//   }, emptyQuery)
// }

export function filterPropIncludes(nodeProp: string[], queryProp: string[]) {
  return queryProp.some((p) =>
    nodeProp.some((pr) => {
      try {
        return pr?.match(new RegExp(p, 'gi'))?.length
      } catch {
        return false
      }
    }),
  )
}

export function filterPropEqualsOne(nodeProp: string[], queryProp: string[]) {
  return queryProp.some((p) => nodeProp.includes(p))
}

export function filterNodeByQuery(
  node: OrgRoamNode,
  query: LinQuery,
  name: string,
  queries: LinQueries,
  pastQueries: string[] = [],
): boolean {
  if (pastQueries.includes(name)) return false
  const processedQueries = [...pastQueries, name]
  return query?.some?.((entry) => {
    const { keyword, value } = entry
    if (keyword !== 'prop' && value?.length === 0) {
      return false
    }
    if (typeof value === 'string') return false
    switch (keyword) {
      case 'title':
        return filterPropIncludes([node.title], value)
      case 'file':
        return filterPropIncludes([node.file], value)
      case 'dir':
        return value.some((v) => path.dirname(node.file)?.match(v)?.length)
      case 'tag':
        return filterPropEqualsOne(node.tags, value)
      //return node.tags?.some((tag) => query?.tags?.includes(tag))
      case 'prop':
        return false
      // (
      //   value.some((prop) => {
      //     const [key, val] = prop
      //     return val.some((v) => node.properties?.[key] === v)
      //   }) ?? false
      // )
      case 'mtime':
        return filterPropEqualsOne((node.properties?.mtime as string).split(' '), value)
      case 'ctime':
        return filterPropEqualsOne((node.properties?.ctime as string).split(' '), value)
      case 'query':
        return value.some((v) =>
          filterNodeByQuery(node, queries?.[v as string]?.query, v, queries, processedQueries),
        )
      default:
        return false
    }
  })
}

export function filterNodesByQuery(nodes: NodeObject[], query: LinQuery, mode: string) {
  if (mode === 'color') {
    return nodes
  }
  const list = mode === 'white' ? true : false

  return nodes.filter((nodeArg) => {
    const node = nodeArg as OrgRoamNode
    return filterNodeByQuery(node, query, queries) === list
  })
}

export function filterNodes(nodes: NodeObject[], queries: LinQueries): NodeObject[] {
  return nodes.filter((nodeArg) => {
    const node = nodeArg as OrgRoamNode
    return !Object.entries(queries).some((entry) => {
      const [name, { query, mode }] = entry
      if (mode === 'color') {
        return false
      }
      const list = mode === 'white' ? true : false
      return filterNodeByQuery(node, query, name, queries) !== list
    })
  }, [])
}

// export function filterNodes(nodes: NodeObject[], queries: LinQueries): NodeObject[] {
//   return Object.entries(queries).reduce<NodeObject[]>((acc, entry) => {
//     const [name, { query, mode }] = entry
//     return filterNodesByQuery(acc, query, mode)
//   }, nodes)
// }
