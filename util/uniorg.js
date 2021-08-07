
import { unified } from 'unified';
var createStream = import('unified-stream');
var uniorgParse = import('uniorg-parse');
var uniorg2rehype = import('uniorg-rehype');
var html = import('rehype-stringify');

var processor = unified().use(uniorgParse).use(uniorg2rehype).use(html);

process.stdin.pipe(createStream(processor)).pipe(process.stdout);
