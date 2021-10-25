import { filter } from '@chakra-ui/react'
import {
  parseQuery,
  Query,
  emptyQuery,
  filterNodesByQuery,
  filterNodes,
  filterNodeByQuery,
  filterPropIncludes,
  filterPropEqualsOne,
} from './parseQuery'

import { createTestNode, testNode } from './queryTestData'

describe('query parsing', () => {
  it('parses linearly', () => {
    expect(parseQuery('title:(tag taggy) tags', {})).toEqual([
      { keyword: 'title', value: ['tag taggy'] },
      { keyword: 'title', value: ['tags'] },
    ])
  })
  describe('parsing function', () => {
    it('parses empty', () => {
      expect(parseQuery('', {})).toEqual([])
    })
    it('parses title', () => {
      expect(parseQuery('title:(title) title:(nother title)', {})).toEqual([
        { keyword: 'title', value: ['title'] },
        { keyword: 'title', value: ['nother title'] },
      ])
    })
    it('parses no syntax as title', () => {
      expect(parseQuery('title', {})).toEqual([{ keyword: 'title', value: ['title'] }])
    })
    it('separates commas', () => {
      expect(parseQuery('title:(t1, t2)', {})).toEqual([{ keyword: 'title', value: ['t1', 't2'] }])
    })
    it('parses tags', () => {
      expect(parseQuery('tag:(tag) tag:(nother tag)', {})).toEqual([
        { keyword: 'tag', value: ['tag'] },
        { keyword: 'tag', value: ['nother tag'] },
      ])
    })
    it('parses filenames', () => {
      expect(parseQuery('file:(file) file:(nother file)', {})).toEqual([
        { keyword: 'file', value: ['file'] },
        { keyword: 'file', value: ['nother file'] },
      ])
    })
    it('parses times', () => {
      expect(parseQuery('mtime:(2222) ctime:(43525)', {})).toEqual([
        { keyword: 'mtime', value: ['2222'] },
        { keyword: 'ctime', value: ['43525'] },
      ])
    })
    it('parses dir', () => {
      expect(parseQuery('dir:(dailies) dir:(journal)', {})).toEqual([
        { keyword: 'dir', value: ['dailies'] },
        { keyword: 'dir', value: ['journal'] },
      ])
    })
    it('parses queries', () => {
      expect(
        parseQuery('query:(test)', {
          test: { mode: 'black', query: [{ keyword: 'title', value: ['testtitle'] }] },
        }),
      ).toEqual([{ keyword: 'title', value: ['testtitle'] }])
    })
    it('parses everything', () => {
      expect(
        parseQuery(
          'empty title:(title) title:(nother title) tag:(tag) tag:(nother tag) file:(file) file:(nother file) query:(test) mtime:(2222) ctime:(43525) dir:(dailies) dir:(journal)',
          {
            test: {
              mode: 'white',
              query: [
                { keyword: 'title', value: ['testtitle'] },
                { keyword: 'ctime', value: ['33'] },
              ],
            },
          },
        ),
      ).toEqual([
        { keyword: 'title', value: ['empty'] },
        { keyword: 'title', value: ['title'] },
        { keyword: 'title', value: ['nother title'] },
        { keyword: 'tag', value: ['tag'] },
        { keyword: 'tag', value: ['nother tag'] },
        { keyword: 'file', value: ['file'] },
        { keyword: 'file', value: ['nother file'] },
        { keyword: 'title', value: ['testtitle'] },
        { keyword: 'ctime', value: ['33'] },
        { keyword: 'mtime', value: ['2222'] },
        { keyword: 'ctime', value: ['43525'] },
        { keyword: 'dir', value: ['dailies'] },
        { keyword: 'dir', value: ['journal'] },
      ])
    })
  })

  // describe('query merger', () => {
  //   it('merges empty queries', () =>
  //     expect(mergeQueries(emptyQuery, emptyQuery)).toEqual(emptyQuery))
  //   it('concats arrays', () => {
  //     expect(
  //       mergeQueries(['props', { fileless: ['true']] address: ['Geert van der Zwaagweg 33'] },
  //         },
  //         ['props', {
  //             fileless: ['false']]
  //             address: ['Parkweg 143b'],
  //           },
  //         },
  //       ),
  //     ).toEqual(['props', {
  //         fileless: ['true', 'false']]
  //         address: ['Geert van der Zwaagweg 33', 'Parkweg 143b'],
  //       },
  //     })
  //   })
  // })

  describe('filtering function', () => {
    describe('big filter', () => {
      const mode = 'black'
      it("doesn't break on empty", () => {
        const query = parseQuery('', {})
        expect(filterNodesByQuery([], query, mode)).toEqual([])
      })
      it('filters title', () => {
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
        expect(filterNodesByQuery(titlenode, query, mode)).toEqual([titlenode[2]])
      })
      it('can do regex', () => {
        const testnodes = [
          createTestNode({ title: 'Ik ben gerard 200' }),
          createTestNode({ title: 'Ik ben gerard 300' }),
          createTestNode({ title: 'Ik ben gerard vierhonderd' }),
        ]
        const query = parseQuery('title:(/\\d/)', {})
        expect(filterNodesByQuery(testnodes, query, mode)).toEqual([testnodes[2]])
      })
    })
    describe('individual filters', () => {
      const list = false
      describe('title', () => {
        it('filters title', () => {
          expect(filterPropIncludes(['Ik ben gerard'], ['ben'])).toBe(true)
          expect(filterPropIncludes(['Ik heet gerard'], ['ben'])).toBe(false)
        })
        it('filters nodes with title', () => {
          const test = createTestNode({ title: 'Ik ben gerard' })
          const query = parseQuery('title:(gerard)', {})
          expect(query).toEqual([{ keyword: 'title', value: ['gerard'] }])
          expect(filterNodeByQuery(test, query, list)).toEqual(list)
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
          expect(query).toEqual([{ keyword: 'tag', value: ['tag1'] }])
          expect(filterNodesByQuery(testnodes, query, 'black')).toEqual([testnodes[1]])
          const q = parseQuery('tag:(tag3) tag:(tag5)', {})
          expect(filterNodesByQuery(testnodes, q, 'black')).toEqual(testnodes)
        })
      })
    })
  })
})
