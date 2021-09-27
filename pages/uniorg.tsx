import unified from 'unified'
import createStream from 'unified-stream'
import uniorgParse from 'uniorg-parse'
import uniorg2rehype from 'uniorg-rehype'
import highlight from 'rehype-highlight'
import katex from 'rehype-katex'
import rehype2react from 'rehype-react'
import React from 'react'

export interface uniorgProps {
  orgText: string
}

const UniOrg = (props: uniorgProps) => {
  const { orgText } = props
  const processor = unified()
    .use(uniorgParse)
    .use(uniorg2rehype)
    .use(katex)
    .use(rehype2react, { createElement: React.createElement })

  return <div>processor.processSync(orgText)</div>
}

export default UniOrg
