import unified from 'unified'
import createStream from 'unified-stream'
import uniorgParse from 'uniorg-parse'
import uniorg2rehype from 'uniorg-rehype'
import highlight from 'rehype-highlight'
import katex from 'rehype-katex'
import rehype2react from 'rehype-react'
import React from 'react'

export interface UniOrgProps {
  orgText: string
}

export const UniOrg = (props: UniOrgProps) => {
  const { orgText } = props
  const processor = unified()
    .use(uniorgParse)
    .use(uniorg2rehype)
    .use(katex)
    .use(rehype2react, { createElement: React.createElement })

    console.log(processor.processSync(orgText))
    return <div> {processor.processSync(orgText).result}</div>
}

