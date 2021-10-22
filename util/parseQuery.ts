import path from 'path'
import { OrgRoamNode } from '../api'
import { NodeObject } from 'force-graph'

export type Props = { [prop: string]: string }
export interface Query {
  list: string
  tags: string[]
  titles: string[]
  files: string[]
  dirs: string[]
  props: Props
  mtimes: string[]
  ctimes: string[]
  todos: string[]
  queries: Query[]
}

const keywordList = ['title', 'tag', 'file', 'dir', 'prop', 'mtime', 'ctime', 'todo', 'query']

const getKeyWords = (query: string, keyword: string) => {
  const its = query.matchAll(new RegExp(`${keyword}:\\(([^\)]*?)\\)`, 'g'))
  const spread = [...its]
  return spread.length ? spread.map((key) => key[1]) : []
}

export function parseQuery(query: string, queries: Query[]): Query {
  return keywordList.reduce<Query>((acc, key) => {
    if (key === 'query') {
      acc['queries'] = getKeyWords(query, 'query').map((q) => queries[q])
      return
    }
    acc[`${key}s`] = getKeyWords(query, key)
    return
  })
}

export function filterNodeByQuery(node: OrgRoamNode, query: Query) {
  return keywordList.some((keyword) => {
    switch (keyword) {
      case 'query':
        return query.queries?.some((q) => filterNodeByQuery(node, q))
      case 'dir':
        return query.dirs?.some((dir) => path.dirname(node.file)?.includes(dir))
      case 'tag':
        return node.tags?.some((tag) => query?.tags?.includes(tag))
      case 'props':
        return Object.keys(query.props)?.some(
          (prop) => node.properties?.[prop] === query.props?.[prop],
        )
      case 'mtime':
      case 'ctime':
        return node.properties?.[keyword] === query[keyword]
      default:
        return node[keyword] === query[keyword]
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
