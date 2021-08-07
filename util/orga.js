const stream = import('unified-stream')
const reorg = require('@orgajs/reorg')
const mutate = require('@orgajs/reorg-rehype')
const html = import('rehype-stringify')

const processor = reorg().use(mutate).use(html)

process.stid.pipe(stream(processor)).pipe(process.stdout)
