import React, { Component } from 'react'

import {Card, CardTitle, CardHeader} from 'material-ui/Card';
import {List, ListItem} from 'material-ui/List';
import Divider from 'material-ui/Divider';

import AuthoredListItem from '../items/AuthoredListItem';
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
        <CardTitle title='Feed'/>
        <Divider />
        <List>
          { (items.length > 0) ?
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
            :
              <ListItem primaryText={"No items to show"} />
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
