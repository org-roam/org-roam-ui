import unified from 'unified';
import parse from 'uniorg-parse';
import uniorg2rehype from 'uniorg-rehype';
import rehypeStringify from 'rehype-stringify';
import React from 'react';

export interface ViewPageProps {

}
export default function ViewPage(props: ViewPageProps) {

    const processor = unified().use(parse)
        .use(uniorg2rehype)
        .use(rehypeStringify);

    processor
        .process(`* org-mode example\n your text goes here`)
        .then((file) => console.log(file.contents));

    return (
        <div>
        </div>
    )
}
