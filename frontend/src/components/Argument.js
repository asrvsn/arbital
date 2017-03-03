import React, { Component, PropTypes } from 'react'

import {Card, CardActions, CardHeader, CardText} from 'material-ui/Card';
import FlatButton from 'material-ui/FlatButton';

import {List, ListItem} from 'material-ui/List';

import AuthoredListItem from './AuthoredListItem';
import GetterHOC from '../hoc/GetterHOC';

const styles = {
  chip: {
    margin: 4,
  },
}

const Argument = (props) => {
  const { argument } = this.props
  const { items } = this.state

  const isMyArgument = true // TODO

  return (
    <Card>
      <CardHeader
        title={argument.summary}
        subtitle={argument.owner}
      />
      { isMyArgument &&
        <CardActions>
          <FlatButton label="Update" />
          <FlatButton label="Delete" />
        </CardActions>
      }
      <CardText>
        <List>
          { items.map(item =>
              <AuthoredListItem
                text={item.claimText}
                authorId={item.claimAuthorId}
                authorName={item.claimAuthorName}
                hrefPath={'/claims/' + item.claimId}
              />
            )
          }
        </List>
      </CardText>
    </Card>
  )
}

export default GetterHOC(
  Argument,
  (props) => ([
    {
      path: props.location.pathName, // TODO
      mapResponseToProps: (resp) => {argument: JSON.parse(resp.body)}
    },
    {
      path: props.location.pathName + '/items', // TODO
      mapResponseToProps: (resp) => {items: JSON.parse(resp.body)}
    }
  ])
)
