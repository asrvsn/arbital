import React, { Component } from 'react'

import {Card, CardTitle, CardHeader} from 'material-ui/Card';
import {List, ListItem} from 'material-ui/List';

import AuthoredListItem from './AuthoredListItem';
import GetterHOC from '../hoc/GetterHOC'

const styles= {
  root: {
    height: '100%',
    width: '100%',
  }
}

const Feed = (props) => {
  const { items, router } = props
  const goToItem = (item) => router.push('/claims/' + item.id)
  const goToAuthor = (item) => router.push('/users/' + item.authorId)

  return (
    <div style={styles.root}>
      <Card>
        <CardHeader title='feed'/>
        <List>
          { (items == []) ?
            <ListItem primaryText={"No items to show"} />
            :
            items.map(item => (
              <AuthoredListItem
                key={item.id}
                text={item.text}
                authorId={item.authorId}
                authorName={item.authorName}
                onTouchTap={() => goToItem(item)}
                onAuthorTouchTap={() => goToAuthor(item)}
              />
            ))
          }
        </List>
      </Card>
    </div>
  )
}

export default GetterHOC(
  Feed,
  (props) => ({
    claims: {
      path: '/',
      mapResponseToProps: (resp) => ({items: resp})
    }
  })
)
