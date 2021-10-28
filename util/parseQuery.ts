import { OrgRoamNode } from '../api'
export type Props = { [prop: string]: string }
export interface Query {
  list: string
  tags: string[]
  titles: string[]
  files: string[]
  dirs: string[]
  props: Props[]
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

export function filterNodesByQuery(nodes: OrgRoamNode[], query: Query) {}
