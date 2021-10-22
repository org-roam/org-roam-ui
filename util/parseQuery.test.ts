import { parseQuery, Query, emptyQuery } from './parseQuery'

describe('parsing function', () => {
  it('parses', () => {
    expect(parseQuery('', {})).toEqual(emptyQuery)
  })
})
