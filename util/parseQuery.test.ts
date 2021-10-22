import { filter } from '@chakra-ui/react'
import {
  parseQuery,
  Query,
  emptyQuery,
  mergeQueries,
  filterNodesByQuery,
  filterNodes,
  filterNodeByQuery,
  filterPropIncludes,
  filterPropEqualsOne,
} from './parseQuery'

import { createTestNode, testNode } from './queryTestData'

describe('query parsing', () => {
  describe('parsing function', () => {
    it('parses empty', () => {
      expect(parseQuery('', {})).toEqual(emptyQuery)
    })
    it('parses titles', () => {
      expect(parseQuery('title:(title) title:(nother title)', {})).toEqual({
        ...emptyQuery,
        titles: ['title', 'nother title'],
      })
    })
    it('parses tags', () => {
      expect(parseQuery('tag:(tag) tag:(nother tag)', {})).toEqual({
        ...emptyQuery,
        tags: ['tag', 'nother tag'],
      })
    })
    it('parses filenames', () => {
      expect(parseQuery('file:(file) file:(nother file)', {})).toEqual({
        ...emptyQuery,
        files: ['file', 'nother file'],
      })
    })
    it('parses times', () => {
      expect(parseQuery('mtime:(2222) ctime:(43525)', {})).toEqual({
        ...emptyQuery,
        mtimes: ['2222'],
        ctimes: ['43525'],
      })
    })
    it('parses dir', () => {
      expect(parseQuery('dir:(dailies) dir:(journal)', {})).toEqual({
        ...emptyQuery,
        dirs: ['dailies', 'journal'],
      })
    })
    it('parses queries', () => {
      expect(
        parseQuery('query:(test)', {
          test: { mode: 'white', query: { ...emptyQuery, titles: ['testtitle'] } },
        }),
      ).toEqual({
        ...emptyQuery,
        titles: ['testtitle'],
      })
    })
  })

  describe('query merger', () => {
    it('merges empty queries', () =>
      expect(mergeQueries(emptyQuery, emptyQuery)).toEqual(emptyQuery))
    it('concats arrays', () => {
      expect(
        mergeQueries({ ...emptyQuery, titles: ['jam'] }, { ...emptyQuery, titles: ['jelly'] }),
      ).toEqual({ ...emptyQuery, titles: ['jam', 'jelly'] })
    })
    it('creates unique keys', () => {
      expect(
        mergeQueries(
          { ...emptyQuery, ctimes: ['000', '111'] },
          { ...emptyQuery, ctimes: ['000', '111'] },
        ),
      ).toEqual({ ...emptyQuery, ctimes: ['000', '111'] })
    })
    it('merges properties', () => {
      expect(
        mergeQueries(
          {
            ...emptyQuery,
            props: { fileless: ['true'], address: ['Geert van der Zwaagweg 33'] },
          },
          {
            ...emptyQuery,
            props: {
              fileless: ['false'],
              address: ['Parkweg 143b'],
            },
          },
        ),
      ).toEqual({
        ...emptyQuery,
        props: {
          fileless: ['true', 'false'],
          address: ['Geert van der Zwaagweg 33', 'Parkweg 143b'],
        },
      })
    })
  })

  describe('filtering function', () => {
    describe('big filter', () => {
      const mode = 'black'
      it("doesn't break on empty", () => {
        const query = parseQuery('', {})
        expect(filterNodesByQuery([], query, mode)).toEqual([])
      })
      it('filters titles', () => {
        const testnodes = [
          createTestNode({ title: 'Ik ben gerard' }),
          createTestNode({ title: 'Ik heet gerard' }),
        ]
        const queries = { ben: { query: parseQuery('title:(ben)', {}), mode } }
        expect(filterNodes(testnodes, queries)).toEqual([testnodes[1]])
      })
      it('is case insensitive', () => {
        const titlenode = [
          createTestNode({ title: 'Ik ben Gerard' }),
          createTestNode({ title: 'Ik ben gerard' }),
          createTestNode({ title: 'Ik ben erard' }),
        ]
        const query = parseQuery('title:(G)', {})
        expect(filterNodesByQuery([titlenode], query, mode)).toEqual([titlenode[2]])
      })
      it('can do regex', () => {
        const testnodes = [
          createTestNode({ title: 'Ik ben gerard 200' }),
          createTestNode({ title: 'Ik ben gerard 300' }),
          createTestNode({ title: 'Ik ben gerard vierhonderd' }),
        ]
        const query = parseQuery('title:(/d/)', {})
        expect(filterNodesByQuery(testnodes, query, mode)).toEqual([testnodes[2]])
      })
    })
    describe('individual filters', () => {
      const list = false
      describe('titles', () => {
        it('filters titles', () => {
          expect(filterPropIncludes(['Ik ben gerard'], ['ben'])).toBe(true)
          expect(filterPropIncludes(['Ik heet gerard'], ['ben'])).toBe(false)
        })
        it('filters nodes with title', () => {
          const test = createTestNode({ title: 'Ik ben gerard' })
          const query = parseQuery('title:(gerard)', {})
          expect(query).toEqual({ ...emptyQuery, titles: ['gerard'] })
          expect(filterNodeByQuery(test, query, list)).toEqual(!list)
          const q = parseQuery('title:(pieter)', {})
          expect(filterNodeByQuery(test, q, list)).toEqual(list)
        })
      })
      describe('tags', () => {
        it('filters tags', () => {
          expect(filterPropEqualsOne(['tag1', 'tag2'], ['tag1', 'tag3'])).toEqual(true)
          expect(filterPropEqualsOne(['tag4', 'tag2'], ['tag1', 'tag3'])).toEqual(false)
        })
        it('filters nodes with tags', () => {
          const testnodes = [
            createTestNode({ tags: ['tag1', 'tag2'] }),
            createTestNode({ tags: ['tag10', 'tag9'] }),
          ]
          const query = parseQuery('tag:(tag1)', {})
          expect(query).toEqual({ ...emptyQuery, tags: ['tag1'] })
          expect(filterNodesByQuery(testnodes, query, 'black')).toEqual([testnodes[1]])
          const q = parseQuery('tag:(tag3) tag:(tag5)', {})
          expect(filterNodesByQuery(testnodes, q, 'black')).toEqual(testnodes)
        })
      })
    })
  })
})
