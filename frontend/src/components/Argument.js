import React, { Component, PropTypes } from 'react'

import {Card, CardActions, CardHeader, CardText} from 'material-ui/Card';
import FlatButton from 'material-ui/FlatButton';

import {List, ListItem} from 'material-ui/List';

import AuthoredListItem from './AuthoredListItem';

const styles = {
  chip: {
    margin: 4,
  },
}

class Argument extends Component {

  componentDidMount() {
    // TODO load argument items into state
  }

  render() {
    const { argument } = this.props
    const { items } = this.state

    // TODO
    const isMyArgument = undefined

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
                />
              )
            }
          </List>
        </CardText>
      </Card>
    )
  }
}

export default Argument
