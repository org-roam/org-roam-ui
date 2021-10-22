import { parseQuery, Query, emptyQuery, mergeQueries } from './parseQuery'

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
        parseQuery('query:(test)', { test: { ...emptyQuery, titles: ['testtitle'] } }),
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

  describe('filtering function', () => {})
})
